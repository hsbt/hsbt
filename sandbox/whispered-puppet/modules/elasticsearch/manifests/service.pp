class elasticsearch::service {
  service {
    'elasticsearch':
      ensure     => running,
      enable     => true,
      hasrestart => true,
      hasstatus  => true,
  }
}
