(defaddon atlassian
  "Provide integration with atlassian tools."

  (defgroup atlassian nil
    "Configuration for Atlassian tools."
    :group 'nomad)

  (defcustom jira-url "http://jira"
    "The address of the JIRA application"
    :type 'string
    :group 'atlassian)

  (defcustom bamboo-url "http://bamboo"
    "The address of the Bamboo application"
    :type 'string
    :group 'atlassian))
