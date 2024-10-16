# Kubed

Kubed is a rich Kubernetes interface within Emacs.  It helps you work
with your Kubernetes clusters and deployments with the full power of
`kubectl`, and with the comfort and confidence of an intuitive
interactive interface.

![Kubed](kubed.png)

![Pod context menu](pod-context-menu.png)

![Resource fields explorer](explain-and-patch.png)

You can use Kubed to:

- Browse and manage Kubernetes workloads
- Connect to pods and edit files or execute commands
- Create new resources, edit and delete them
- Work with multiple clusters
- Get help about various Kubernetes objects
- ...

# Getting Started

Get Kubed from GNU ELPA with `M-x package-install`, or use your
favorite Emacs package manager to install Kubed from Git.  You can
clone the Kubed Git repository from any of the following locations:

- https://git.sr.ht/~eshel/kubed
- https://github.com/eshelyaron/kubed.git
- git://git.eshelyaron.com/kubed.git

To get started with Kubed, all you need is `kubectl` and Emacs.  Bind
`kubed-prefix-map` to a convenient key (e.g. `C-c k` or `M-K`), or use
`M-x kubed-transient` to explore some of the available commands.

For more information about usage and customization, see the Kubed
manual in Info or online at https://eshelyaron.com/kubed.html
