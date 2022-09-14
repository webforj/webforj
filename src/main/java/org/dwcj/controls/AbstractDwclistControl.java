package org.dwcj.controls;

import java.util.HashMap;
import java.util.Map;

public abstract class AbstractDwclistControl extends AbstractDwcControl {

    protected Map<Object, String> values = new HashMap<>();

    abstract protected void populate();

    @Override
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();
    }


}
