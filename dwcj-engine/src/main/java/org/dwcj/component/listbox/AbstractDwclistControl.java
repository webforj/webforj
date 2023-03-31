package org.dwcj.component.listbox;

import java.util.HashMap;
import java.util.Map;

import org.dwcj.component.AbstractDwcControl;

import com.basis.startup.type.BBjVector;


public abstract class AbstractDwclistControl extends AbstractDwcControl {

    protected BBjVector data2 = new BBjVector();
    protected Map<Object, String> values = new HashMap<>();

    protected abstract void populate();

}
