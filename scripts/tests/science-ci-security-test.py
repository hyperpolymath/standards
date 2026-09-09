#!/usr/bin/env python3
# SPDX-License-Identifier: MPL-2.0
"""Exercise the workflow's actual shell steps with controlled remote data."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

import yaml

ROOT = Path(__file__).resolve().parents[2]
PUBLIC_KEY = "gitlab.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAfuCHKVTjquxvt6CM6tdG4SLp1Btn/nOeHHE5UOzRdf\n"
FINGERPRINT = "SHA256:eUXGGm1YGsMAS7vkcx6JOJdOGHPem5gQp4taiCfCLB8"


class MirrorHostVerification(unittest.TestCase):
    def test_every_ssh_mirror_verifies_before_push(self):
        workflow = yaml.safe_load((ROOT / ".github/workflows/mirror-reusable.yml").read_text())
        for name, job in workflow["jobs"].items():
            if name == "mirror-radicle":
                continue
            verify = next(s for s in job["steps"] if s.get("name", "").startswith("Verify "))
            push = next(s for s in job["steps"] if "git push" in s.get("run", ""))
            self.assertLess(job["steps"].index(verify), job["steps"].index(push))
            self.assertEqual(verify["if"], push["if"])
            self.assertFalse(verify.get("continue-on-error", False))
            with tempfile.TemporaryDirectory() as directory:
                tmp = Path(directory)
                stub = tmp / "ssh-keyscan"
                stub.write_text('#!/bin/sh\nprintf "%s" "$TEST_HOST_KEY"\n')
                stub.chmod(0o755)
                env = dict(os.environ, PATH=f"{tmp}:{os.environ['PATH']}", RUNNER_TEMP=directory,
                           GITHUB_ENV=str(tmp / "env"), MIRROR_HOST="gitlab.com",
                           APPROVED_FINGERPRINT=FINGERPRINT)
                for key, fingerprint, succeeds in [
                    (PUBLIC_KEY, FINGERPRINT, True),
                    (PUBLIC_KEY.replace("OzRdf", "OzRdg"), FINGERPRINT, False),
                    ("", FINGERPRINT, False),
                    ("invalid key", FINGERPRINT, False),
                    (PUBLIC_KEY, "", False),
                ]:
                    with self.subTest(job=name, key=key[-8:], fingerprint=fingerprint):
                        (tmp / "env").unlink(missing_ok=True)
                        result = subprocess.run(["bash", "-c", verify["run"]],
                            env=dict(env, TEST_HOST_KEY=key, APPROVED_FINGERPRINT=fingerprint),
                            capture_output=True, text=True)
                        self.assertEqual(result.returncode == 0, succeeds, result.stderr)
                        self.assertEqual((tmp / "env").exists(), succeeds)
                        if succeeds:
                            self.assertIn("StrictHostKeyChecking=yes", (tmp / "env").read_text())


class ScannerSourceVerification(unittest.TestCase):
    def test_resolved_commit_survives_remote_advancing_and_cache_mismatch_fails(self):
        workflow = yaml.safe_load((ROOT / ".github/workflows/hypatia-scan-reusable.yml").read_text())
        step = next(s for s in workflow["jobs"]["scan"]["steps"]
                    if s.get("name") == "Check out resolved Hypatia commit")
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            upstream = tmp / "upstream"
            subprocess.run(["git", "init", "-q", str(upstream)], check=True)
            def commit(text):
                (upstream / "source").write_text(text)
                subprocess.run(["git", "-C", str(upstream), "add", "source"], check=True)
                subprocess.run(["git", "-C", str(upstream), "-c", "user.name=CI Test",
                    "-c", "user.email=ci@example.invalid", "-c", "commit.gpgsign=false",
                    "commit", "-qm", text], check=True)
                return subprocess.check_output(["git", "-C", str(upstream), "rev-parse", "HEAD"], text=True).strip()
            resolved = commit("resolved")
            newer = commit("advanced")
            source = tmp / "scanner"
            script = step["run"].replace('$HOME/hypatia', str(source)).replace(
                "https://github.com/hyperpolymath/hypatia.git", str(upstream))
            env = dict(os.environ, HYPATIA_SHA=resolved)
            for expected in (0, 0):
                result = subprocess.run(["bash", "-c", script], env=env, capture_output=True)
                self.assertEqual(result.returncode, expected, result.stderr)
            self.assertEqual((source / "source").read_text(), "resolved")
            result = subprocess.run(["bash", "-c", script], env=dict(env, HYPATIA_SHA=newer), capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn(b"cached source does not match", result.stdout)


class PolicyStartupVerification(unittest.TestCase):
    def test_commented_jobs_and_retired_policy_fail_with_positive_controls(self):
        with tempfile.TemporaryDirectory() as directory:
            tmp = Path(directory)
            workflow = tmp / ".github/workflows/ci.yml"
            workflow.parent.mkdir(parents=True)
            subprocess.run(["git", "init", "-q", directory], check=True)
            workflow.write_text("name: CI\non: push\njobs:\n  # test:\n  #   runs-on: ubuntu-latest\n")
            subprocess.run(["git", "-C", directory, "add", "."], check=True)
            parser = ROOT / "tools/policy/check-workflows-parse.sh"
            result = subprocess.run(["bash", str(parser)], cwd=directory, capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            workflow.write_text("name: CI\non: push\njobs:\n  test:\n    runs-on: ubuntu-latest\n    steps:\n      - run: test -f .machine_readable/STATE.a2ml\n")
            checker = ROOT / "scripts/check-descriptile-policy.sh"
            result = subprocess.run(["bash", str(checker)], cwd=directory, capture_output=True)
            self.assertNotEqual(result.returncode, 0)
            workflow.write_text(workflow.read_text().replace(".machine_readable/STATE", ".machine_readable/descriptiles/STATE"))
            for check in (parser, checker):
                result = subprocess.run(["bash", str(check)], cwd=directory, capture_output=True)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main()
