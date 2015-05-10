(require 'nomad-config)
(require 'nomad-loader)
(require 'nomad-packages)

(nomad/include custom-file)
(nomad/include defaults-file)
(nomad/include local-defaults-file)

(when enable-nomad-addons
  (require 'nomad-addons))

(provide 'nomad)
