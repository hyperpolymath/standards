# SPDX-License-Identifier: AGPL-3.0-or-later
# Homebrew formula for K9 SVC
#
# To install:
#   brew tap hyperpolymath/k9
#   brew install k9-svc
#
# Or directly:
#   brew install --HEAD hyperpolymath/k9/k9-svc

class K9Svc < Formula
  desc "Self-Validating Components - a file format that eats its own dog food"
  homepage "https://github.com/hyperpolymath/k9-svc"
  url "https://github.com/hyperpolymath/k9-svc/archive/refs/tags/v1.0.0.tar.gz"
  sha256 "82e51dec2891728d233392f6e84004e5d6060bf2c936dd47445d59ca66c09f94"
  license "AGPL-3.0-or-later"
  head "https://github.com/hyperpolymath/k9-svc.git", branch: "main"

  depends_on "nickel"
  depends_on "just"
  depends_on "openssl@3"

  def install
    # Install scripts
    bin.install "must" => "k9-must"
    bin.install "sign.sh" => "k9-sign"

    # Install schemas and support files
    (share/"k9").install "pedigree.ncl"
    (share/"k9").install "register.ncl"
    (share/"k9").install "leash.ncl"
    (share/"k9").install "justfile"

    # Install examples
    (share/"k9/examples").install Dir["examples/*"]

    # Install assets
    (share/"k9/assets").install Dir["assets/*.svg"]

    # Install documentation
    doc.install "README.adoc"
    doc.install "SPEC.adoc"
    doc.install "GUIDE.adoc"

    # Create wrapper script
    (bin/"k9").write <<~EOS
      #!/bin/sh
      # K9 SVC wrapper - runs just with K9 recipes
      exec just --justfile #{share}/k9/justfile "$@"
    EOS
  end

  def post_install
    # Note about MIME setup
    ohai "K9 SVC installed!"
    ohai "Run 'k9 setup-mime' to register MIME types"
  end

  test do
    # Test must shim
    assert_match "K9 Environment", shell_output("#{bin}/k9-must status")

    # Test schema validation
    system "nickel", "typecheck", "#{share}/k9/pedigree.ncl"

    # Test key generation (in temp dir)
    ENV["XDG_CONFIG_HOME"] = testpath
    system "#{bin}/k9-sign", "keygen", "test"
    assert_predicate testpath/"k9/keys/test.key", :exist?
    assert_predicate testpath/"k9/keys/test.pub", :exist?
  end
end
