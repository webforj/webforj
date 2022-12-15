package org.dwcj.controls.listbox;

import java.util.HashMap;
import java.util.Map;

import org.dwcj.controls.AbstractDwcControl;

import com.basis.startup.type.BBjVector;


public abstract class AbstractDwclistControl extends AbstractDwcControl {

    protected BBjVector data2 = new BBjVector();
    protected Map<Object, String> values = new HashMap<>();



    

    protected abstract void populate();

}
