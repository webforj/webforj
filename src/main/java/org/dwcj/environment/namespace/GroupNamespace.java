package org.dwcj.environment.namespace;

import org.dwcj.Environment;

public class GroupNamespace extends StandardNamespace implements Namespace
{

    public GroupNamespace() {
        ns = Environment.getInstance().getBBjAPI().getGroupNamespace();
    }
}
