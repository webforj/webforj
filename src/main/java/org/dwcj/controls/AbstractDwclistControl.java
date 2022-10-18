package org.dwcj.controls;

import java.util.HashMap;
import java.util.Map;

import com.basis.startup.type.BBjVector;


public abstract class AbstractDwclistControl extends AbstractDwcControl {

    protected BBjVector data2 = new BBjVector();
    protected Map<Object, String> values = new HashMap<>();



    

    abstract protected void populate();

    @Override
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();
    }


}
