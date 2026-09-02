# Ruby's YJIT/ZJIT keep raising the minimum rustc, so the toolchain comes
# from rustup rather than Ubuntu's deb. Run `rustup update` by hand before
# building a Ruby; keeping the toolchain current is not a converge step.
execute "install rustup" do
  command "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --profile minimal"
  user "ubuntu"
  cwd "/home/ubuntu"
  not_if "test -x /home/ubuntu/.cargo/bin/rustup"
end
