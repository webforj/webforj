package org.dwcj.environment.namespace;

import org.dwcj.Environment;

public class GlobalNamespace extends StandardNamespace implements Namespace {

    public GlobalNamespace() {
        ns = Environment.getInstance().getBBjAPI().getGlobalNamespace();
    }
}
