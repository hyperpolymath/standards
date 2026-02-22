#!/usr/bin/env python3
"""
Repo Reconciliation Tool
Compares GitLab and GitHub repositories to identify sync gaps.

Usage:
  export GITLAB_TOKEN="your-token"
  export GITHUB_TOKEN="your-token"
  python repo-reconcile.py

Outputs a reconciliation report showing:
  - Repos only on GitLab (need to mirror to GitHub)
  - Repos only on GitHub (new creations or need to mirror to GitLab)
  - Repos on both (check if synced or diverged)
"""

import os
import json
import requests
from datetime import datetime
from dataclasses import dataclass
from typing import Optional

# Configuration
GITLAB_USER = "hyperpolymath"
GITLAB_GROUPS = ["maa-framework"]  # Add more groups as needed
GITHUB_USER = "hyperpolymath"

@dataclass
class Repo:
    name: str
    platform: str
    url: str
    last_activity: Optional[str]
    commit_count: Optional[int] = None
    description: Optional[str] = None

def get_gitlab_repos(token: str) -> list[Repo]:
    """Fetch all repos from GitLab user and groups."""
    repos = []
    headers = {"PRIVATE-TOKEN": token}

    # User repos
    page = 1
    while True:
        url = f"https://gitlab.com/api/v4/users/{GITLAB_USER}/projects?per_page=100&page={page}"
        resp = requests.get(url, headers=headers)
        if resp.status_code != 200:
            print(f"GitLab API error: {resp.status_code}")
            break
        data = resp.json()
        if not data:
            break
        for r in data:
            repos.append(Repo(
                name=r["path"],
                platform="gitlab",
                url=r["web_url"],
                last_activity=r.get("last_activity_at"),
                description=r.get("description")
            ))
        page += 1

    # Group repos
    for group in GITLAB_GROUPS:
        page = 1
        while True:
            url = f"https://gitlab.com/api/v4/groups/{group}/projects?per_page=100&page={page}&include_subgroups=true"
            resp = requests.get(url, headers=headers)
            if resp.status_code != 200:
                break
            data = resp.json()
            if not data:
                break
            for r in data:
                repos.append(Repo(
                    name=r["path"],
                    platform="gitlab",
                    url=r["web_url"],
                    last_activity=r.get("last_activity_at"),
                    description=r.get("description")
                ))
            page += 1

    return repos

def get_github_repos(token: str) -> list[Repo]:
    """Fetch all repos from GitHub user."""
    repos = []
    headers = {"Authorization": f"token {token}"}

    page = 1
    while True:
        url = f"https://api.github.com/users/{GITHUB_USER}/repos?per_page=100&page={page}"
        resp = requests.get(url, headers=headers)
        if resp.status_code != 200:
            print(f"GitHub API error: {resp.status_code}")
            break
        data = resp.json()
        if not data:
            break
        for r in data:
            repos.append(Repo(
                name=r["name"],
                platform="github",
                url=r["html_url"],
                last_activity=r.get("pushed_at"),
                description=r.get("description")
            ))
        page += 1

    return repos

def reconcile(gitlab_repos: list[Repo], github_repos: list[Repo]) -> dict:
    """Compare repos and categorize sync status."""
    gitlab_names = {r.name: r for r in gitlab_repos}
    github_names = {r.name: r for r in github_repos}

    all_names = set(gitlab_names.keys()) | set(github_names.keys())

    result = {
        "gitlab_only": [],
        "github_only": [],
        "both_synced": [],
        "both_diverged": [],
        "unknown": []
    }

    for name in sorted(all_names):
        gl = gitlab_names.get(name)
        gh = github_names.get(name)

        if gl and not gh:
            result["gitlab_only"].append({
                "name": name,
                "gitlab_url": gl.url,
                "last_activity": gl.last_activity,
                "action": "Mirror to GitHub"
            })
        elif gh and not gl:
            result["github_only"].append({
                "name": name,
                "github_url": gh.url,
                "last_activity": gh.last_activity,
                "action": "New on GitHub (mirror to GitLab?)"
            })
        else:
            # Both exist - check if diverged
            gl_time = gl.last_activity or ""
            gh_time = gh.last_activity or ""

            # Simple heuristic: if times differ by > 1 day, flag as potentially diverged
            try:
                gl_dt = datetime.fromisoformat(gl_time.replace("Z", "+00:00"))
                gh_dt = datetime.fromisoformat(gh_time.replace("Z", "+00:00"))
                diff = abs((gl_dt - gh_dt).days)

                if diff > 1:
                    ahead = "GitLab" if gl_dt > gh_dt else "GitHub"
                    result["both_diverged"].append({
                        "name": name,
                        "gitlab_url": gl.url,
                        "github_url": gh.url,
                        "gitlab_activity": gl_time,
                        "github_activity": gh_time,
                        "ahead": ahead,
                        "days_apart": diff,
                        "action": f"Check sync - {ahead} is {diff} days ahead"
                    })
                else:
                    result["both_synced"].append({
                        "name": name,
                        "status": "Likely synced"
                    })
            except:
                result["unknown"].append({
                    "name": name,
                    "gitlab_url": gl.url if gl else None,
                    "github_url": gh.url if gh else None,
                    "action": "Could not compare dates"
                })

    return result

def print_report(result: dict):
    """Print human-readable reconciliation report."""
    print("\n" + "="*70)
    print("REPOSITORY RECONCILIATION REPORT")
    print("="*70)

    print(f"\n## GITLAB ONLY ({len(result['gitlab_only'])} repos)")
    print("These repos exist on GitLab but NOT on GitHub:")
    for r in result["gitlab_only"][:20]:  # Show first 20
        print(f"  - {r['name']}: {r['gitlab_url']}")
    if len(result["gitlab_only"]) > 20:
        print(f"  ... and {len(result['gitlab_only']) - 20} more")

    print(f"\n## GITHUB ONLY ({len(result['github_only'])} repos)")
    print("These repos exist on GitHub but NOT on GitLab:")
    for r in result["github_only"][:20]:
        print(f"  - {r['name']}: {r['github_url']}")
    if len(result["github_only"]) > 20:
        print(f"  ... and {len(result['github_only']) - 20} more")

    print(f"\n## POTENTIALLY DIVERGED ({len(result['both_diverged'])} repos)")
    print("These repos exist on both but may be out of sync:")
    for r in result["both_diverged"]:
        print(f"  - {r['name']}: {r['ahead']} ahead by {r['days_apart']} days")

    print(f"\n## LIKELY SYNCED ({len(result['both_synced'])} repos)")

    print("\n" + "="*70)
    print("SUMMARY")
    print("="*70)
    print(f"  GitLab only:      {len(result['gitlab_only']):4d} (need to mirror to GitHub)")
    print(f"  GitHub only:      {len(result['github_only']):4d} (new on GitHub)")
    print(f"  Diverged:         {len(result['both_diverged']):4d} (need sync check)")
    print(f"  Synced:           {len(result['both_synced']):4d} (OK)")
    print(f"  Unknown:          {len(result['unknown']):4d}")

def main():
    gitlab_token = os.environ.get("GITLAB_TOKEN")
    github_token = os.environ.get("GITHUB_TOKEN")

    if not gitlab_token or not github_token:
        print("Please set GITLAB_TOKEN and GITHUB_TOKEN environment variables")
        print("\nTo get tokens:")
        print("  GitLab: Settings > Access Tokens > Create with 'read_api' scope")
        print("  GitHub: Settings > Developer settings > Personal access tokens")
        return

    print("Fetching GitLab repos...")
    gitlab_repos = get_gitlab_repos(gitlab_token)
    print(f"  Found {len(gitlab_repos)} GitLab repos")

    print("Fetching GitHub repos...")
    github_repos = get_github_repos(github_token)
    print(f"  Found {len(github_repos)} GitHub repos")

    print("Reconciling...")
    result = reconcile(gitlab_repos, github_repos)

    print_report(result)

    # Save full report as JSON
    with open("reconciliation-report.json", "w") as f:
        json.dump(result, f, indent=2, default=str)
    print("\nFull report saved to reconciliation-report.json")

if __name__ == "__main__":
    main()


# Quick check mode - just test a list of repos
def quick_check_github(repos: list[str]) -> dict:
    """Check if repos exist on GitHub without auth."""
    import urllib.request
    results = {"exists": [], "missing": []}
    for repo in repos:
        url = f"https://github.com/hyperpolymath/{repo}"
        try:
            req = urllib.request.Request(url, method='HEAD')
            urllib.request.urlopen(req, timeout=5)
            results["exists"].append(repo)
        except:
            results["missing"].append(repo)
    return results

if __name__ == "__main__" and len(__import__('sys').argv) > 1:
    # Quick mode: python repo-reconcile.py check repo1 repo2 repo3
    if __import__('sys').argv[1] == "check":
        repos = __import__('sys').argv[2:]
        print(f"Checking {len(repos)} repos against GitHub...")
        results = quick_check_github(repos)
        print(f"\n✓ EXISTS on GitHub ({len(results['exists'])}):")
        for r in results['exists']:
            print(f"  {r}")
        print(f"\n✗ MISSING on GitHub ({len(results['missing'])}):")
        for r in results['missing']:
            print(f"  {r}")
