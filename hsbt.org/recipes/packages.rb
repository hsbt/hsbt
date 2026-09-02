# Grouped by what needs them; overlap between groups is fine because
# package is idempotent.

# h2o is built from source in ~/build/h2o (see hsbt-org-h2o-source-build).
# bison is for the bundled mruby's parser.
h2o_build = %w[build-essential cmake bison libssl-dev zlib1g-dev]

# ruby-build via rbenv for the tDiary runtime.
ruby_build = %w[build-essential autoconf patch libssl-dev libyaml-dev libffi-dev libgmp-dev zlib1g-dev]

# tdiary.service runs index.fcgi under spawn-fcgi. The fcgi gem builds
# against libfcgi-dev; idn-ruby builds against libidn-dev and needs
# libidn12 at runtime (autoremove must not take it).
tdiary_runtime = %w[spawn-fcgi libfcgi-dev libidn-dev libidn12]

# certbot renews the TLS certificate; s3cmd is for backups.
site_ops = %w[certbot s3cmd]

# Interactive tools for when someone is logged in.
cli_tools = %w[git curl vim ripgrep]

(h2o_build + ruby_build + tdiary_runtime + site_ops + cli_tools).uniq.each do |name|
  package name
end
