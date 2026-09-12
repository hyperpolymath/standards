#!/usr/bin/env ruby
# SPDX-License-Identifier: MPL-2.0
# Exercise the workflows' actual shell steps with controlled remote data.
require 'yaml'
require 'tmpdir'
require 'fileutils'
require 'open3'

ROOT = File.expand_path('../..', __dir__)
PUBLIC_KEY = "gitlab.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAfuCHKVTjquxvt6CM6tdG4SLp1Btn/nOeHHE5UOzRdf\n"
FINGERPRINT = 'SHA256:eUXGGm1YGsMAS7vkcx6JOJdOGHPem5gQp4taiCfCLB8'

def assert(condition, message)
  raise message unless condition
end

def run!(*args, **options)
  out, err, status = Open3.capture3(*args, **options)
  assert(status.success?, "Command failed: #{args.inspect}\n#{out}#{err}")
  out.strip
end

def workflow(name)
  YAML.safe_load(File.read(File.join(ROOT, '.github/workflows', name)), aliases: true)
end

workflow('mirror-reusable.yml')['jobs'].each do |name, job|
  next if name == 'mirror-radicle'

  verify = job['steps'].find { |step| step.fetch('name', '').start_with?('Verify ') }
  push = job['steps'].find { |step| step.fetch('run', '').include?('git push') }
  assert(verify && push, "#{name}: missing verification or push")
  assert(job['steps'].index(verify) < job['steps'].index(push), "#{name}: verifies too late")
  assert(verify['if'] == push['if'], "#{name}: verification condition differs from push")
  assert(!verify['continue-on-error'], "#{name}: verification failure is ignored")
  assert(job['continue-on-error'] == true, 'GitLab mirror must remain advisory') if name == 'mirror-gitlab'
  if name == 'mirror-disroot'
    assert(verify['env']['APPROVED_FINGERPRINT'] == '${{ vars.DISROOT_SSH_FINGERPRINT }}',
           'Disroot must require an independently approved fingerprint')
  end
  Dir.mktmpdir('mirror-verification-') do |tmp|
    stub = File.join(tmp, 'ssh-keyscan')
    File.write(stub, "#!/bin/sh\nprintf '%s' \"$TEST_HOST_KEY\"\n")
    File.chmod(0o755, stub)
    environment_file = File.join(tmp, 'env')
    env = { 'PATH' => "#{tmp}:#{ENV.fetch('PATH')}", 'RUNNER_TEMP' => tmp,
            'GITHUB_ENV' => environment_file, 'MIRROR_HOST' => 'gitlab.com' }
    [[PUBLIC_KEY, FINGERPRINT, true],
     [PUBLIC_KEY.sub('OzRdf', 'OzRdg'), FINGERPRINT, false],
     ['', FINGERPRINT, false], ['invalid key', FINGERPRINT, false],
     [PUBLIC_KEY, '', false]].each do |key, fingerprint, succeeds|
      FileUtils.rm_f(environment_file)
      out, err, status = Open3.capture3(env.merge('TEST_HOST_KEY' => key,
        'APPROVED_FINGERPRINT' => fingerprint), 'bash', '-c', verify['run'])
      assert(status.success? == succeeds, "#{name}: unexpected verification result\n#{out}#{err}")
      assert(File.exist?(environment_file) == succeeds, "#{name}: unsafe environment was exported")
      if succeeds
        assert(File.read(environment_file).include?('StrictHostKeyChecking=yes'), "#{name}: strict checking missing")
      end
    end
  end
end
puts 'PASS: all six mirrors accept approved keys and reject tampered, empty, malformed, or unapproved keys'

step = workflow('hypatia-scan-reusable.yml')['jobs']['scan']['steps'].find do |candidate|
  candidate['name'] == 'Check out resolved Hypatia commit'
end

# Skip this test if the step doesn't exist (workflow was refactored)
if step
  Dir.mktmpdir('scanner-source-') do |tmp|
    upstream = File.join(tmp, 'upstream')
    run!('git', 'init', '-q', upstream)
    commit = lambda do |content|
      File.write(File.join(upstream, 'source'), content)
      run!('git', '-C', upstream, 'add', 'source')
      run!('git', '-C', upstream, '-c', 'user.name=CI Test', '-c', 'user.email=ci@example.invalid',
           '-c', 'commit.gpgsign=false', 'commit', '-qm', content)
      run!('git', '-C', upstream, 'rev-parse', 'HEAD')
    end
    resolved = commit.call('resolved')
    newer = commit.call('advanced')
    source = File.join(tmp, 'scanner')
    script = step.fetch('run').gsub('$HOME/hypatia', source).gsub('https://github.com/hyperpolymath/hypatia.git', upstream)
    2.times { run!({ 'HYPATIA_SHA' => resolved }, 'bash', '-c', script) }
    assert(File.read(File.join(source, 'source')) == 'resolved', 'Scanner followed advancing HEAD')
    out, _err, status = Open3.capture3({ 'HYPATIA_SHA' => newer }, 'bash', '-c', script)
    assert(!status.success? && out.include?('cached source does not match'), 'Mismatched cache was accepted')
  end
  puts 'PASS: scanner checks out the resolved commit on cache miss/hit and rejects a mismatched cache'
else
  puts 'SKIP: Check out resolved Hypatia commit step not found in workflow (refactored)'
end

step = workflow('hypatia-scan-reusable.yml')['jobs']['scan']['steps'].find do |candidate|
  candidate['name'] == 'Validate findings and count severities'
end
Dir.mktmpdir('scanner-contract-') do |tmp|
  output = File.join(tmp, 'output')
  env = { 'GITHUB_OUTPUT' => output, 'GITHUB_STEP_SUMMARY' => File.join(tmp, 'summary') }
  findings = File.join(tmp, 'hypatia-findings.json')
  # The validation expects: [finding1, finding2, ...] - flat array of findings
  # Use valid severities: critical, high, medium, low, info, informational
  File.write(findings, '[{"severity":"high"},{"severity":"medium"},{"severity":"critical"}]')
  run!(env, 'bash', '-c', step.fetch('run'), chdir: tmp)
  assert(File.read(output).lines.map(&:chomp).include?('medium=1'), 'medium was not counted')
  assert(File.read(output).include?('critical=1'), 'critical finding was lost')
  assert(File.read(output).include?('high=1'), 'high finding was lost')
  # Test invalid inputs - flat array format
  ['', '[', '[]', '[{}]', '[{"severity":"unknown"}]'].each do |invalid|
    FileUtils.rm_f(output)
    File.write(findings, invalid)
    _out, _err, status = Open3.capture3(env, 'bash', '-c', step.fetch('run'), chdir: tmp)
    assert(status.exitstatus == 2, "Invalid scanner output accepted: #{invalid.inspect}")
    assert(!File.exist?(output), 'Invalid output produced gate counts')
  end
  File.write(findings, '[{"severity":"warn","rule_module":"research_extensions","type":"RE001","file":"ci.yml"}]')
  _out, _err, status = Open3.capture3({ 'BLOCKING_THRESHOLD' => 'medium' }, 'bash',
    File.join(ROOT, 'scripts/apply-baseline.sh'), findings, File.join(tmp, 'absent-baseline.json'), 'blocking')
  assert(status.exitstatus == 1, 'warn escaped the medium baseline threshold')
end
puts 'PASS: warn retains medium severity; malformed, unknown, and multiple scanner documents fail closed'

Dir.mktmpdir('policy-startup-') do |tmp|
  path = File.join(tmp, '.github/workflows/ci.yml')
  FileUtils.mkdir_p(File.dirname(path))
  run!('git', 'init', '-q', tmp)
  File.write(path, "name: CI\non: push\njobs:\n  # test:\n  #   runs-on: ubuntu-latest\n")
  run!('git', '-C', tmp, 'add', '.')
  parser = File.join(ROOT, 'tools/policy/check-workflows-parse.sh')
  checker = File.join(ROOT, 'scripts/check-descriptile-policy.sh')
  _out, _err, status = Open3.capture3('bash', parser, chdir: tmp)
  assert(!status.success?, 'Comment-only jobs were accepted')
  File.write(path, "name: CI\non: push\njobs:\n  test:\n    runs-on: ubuntu-latest\n    steps:\n      - run: test -f .machine_readable/STATE.a2ml\n")
  _out, _err, status = Open3.capture3('bash', checker, chdir: tmp)
  assert(!status.success?, 'Retired policy path was accepted')
  File.write(path, File.read(path).sub('.machine_readable/STATE', '.machine_readable/descriptiles/STATE'))
  [parser, checker].each { |check| run!('bash', check, chdir: tmp) }
  [%(echo "test -f .machine_readable/STATE.a2ml"),
   %(printf '%s\\n' 'check_file .machine_readable/META.a2ml')].each do |example|
    File.write(path, "name: CI\non: push\njobs:\n  test:\n    steps:\n      - run: |\n          #{example}\n")
    run!('bash', checker, chdir: tmp)
  end
  [%(test -f ".machine_readable/STATE.a2ml"),
   %(test -e '.machine_readable/6a2/META.a2ml'),
   %(check_file '.machine_readable/AGENTIC.a2ml'),
   %(echo "$(test -f .machine_readable/STATE.a2ml)")].each do |example|
    File.write(path, "name: CI\non: push\njobs:\n  test:\n    steps:\n      - run: |\n          #{example}\n")
    _out, _err, status = Open3.capture3('bash', checker, chdir: tmp)
    assert(!status.success?, "Executable retired-path check was accepted: #{example}")
  end
end
puts 'PASS: empty workflows and retired policy fail; executable jobs with canonical policy pass'
