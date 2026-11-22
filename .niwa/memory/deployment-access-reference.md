---
name: "Deployment access and diagnostic references"
description: "Use the recorded SSH configuration and deployment pointers to locate access details, then recheck current configuration and authority."
type: reference
modified: 2026-11-21
---

# Deployment access and diagnostic references

## SSH configuration

The authoritative machine-local connection details are in `~/.ssh/config`.

As of 2026-11-21 (after the niwa cutover, checked by logging in):

- `niwa-host` (`192.168.24.43`, root, `~/.ssh/id_ed25519_mevedel_lan`) is
  the rebuilt Emacs host CT (Ubuntu 26.04, Proxmox CT 102 reused, same IP,
  new host key).
- `niwa-relay` (`192.168.24.41`, root, same key) is the YunoHost relay server.
- The old `mevedel-host` and `mevedel-relay` aliases were removed.

Use the configured alias rather than a bare user@IP: it pins the key with
`IdentitiesOnly yes` (an earlier test found bare batch logins failing).
Inspect the current SSH configuration before use; this dated observation
does not establish present reachability or credentials.

Current layout on `niwa-host`: kit in `/home/niwa/niwa-host` (owner `niwa`,
rootless Podman), containers `niwa-private`/`niwa-public` (daemon
`emacsclient` socket `niwa`), user units
`niwa-host.service`, `niwa-host-update.timer` (nightly 4:00),
`niwa-host-refs.timer`; root reaches them with `systemctl --user -M niwa@`.
On the relay server: `niwa-relay.service` on `127.0.0.1:7466` behind
`niwa.skadonk.me`, `relay-update.timer`.

## Deployment and diagnostic pointers

The host kit is maintained in `~/Projekte/niwa-host` (`FrauH0lle/niwa-host`).
Roll out kit changes with `./deploy niwa-host` from its `master`. The
desktop source checkout is `~/Projekte/niwa.el` (moved from
`~/Projekte/mevedel` on 2026-11-21); the running package uses
`~/.emacs.d/straight/build/niwa`, built from that checkout.
The desktop relay setting lives in `~/.emacs.d/site-lisp/secrets.el`
(`wss://niwa.skadonk.me`). Repository diagnostic tooling:
`benchmark/collab-latency/`.

The relay was recorded on 2026-08-18 as deployed on Roland's YunoHost server and used from iPhones. Browser/phone collaboration is actual usage, not just a demonstration scenario. Current build and viewer contracts belong in `relay/AGENTS.md` and `relay/README.md`, not this deployment snapshot.

**How to apply:** use these pointers to locate the deployment and its diagnostics, then verify current aliases, service/container layout, and installed versions. These are dated references, not authorization to deploy, restart services, or mutate remote state.

**Earlier source:** Claude project memories `mevedel-host-access.md` and `collab-relay-deployment.md` under `/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`, curated for import at the user's request on 2026-10-19. No host connection was made during that import. Historical versions, backup inventories, deployment commands, and Claude permission rules were omitted.
