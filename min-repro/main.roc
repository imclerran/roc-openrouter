app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
}

import Module { task: (Task.ok {}) }

main = Task.loop! {} loop

loop = \{} -> Task.ok (Step Module.effect!)
