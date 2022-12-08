package org.dwcj.controls.checkablemenuitem;

import org.dwcj.controls.menuitem.MenuItem;

import com.basis.bbj.proxies.sysgui.BBjCheckableMenuItem;
import com.basis.startup.type.BBjException;

public class CheckableMenuItem extends MenuItem {

    private BBjCheckableMenuItem bbjCheckableMenuItem;

    public CheckableMenuItem setSelected(boolean selected) {
        try {
            bbjCheckableMenuItem.setSelected(selected);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;

    }

    public boolean isSelected() {
        try {
            return bbjCheckableMenuItem.isSelected();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }

    @Override
    public CheckableMenuItem setID(String id){
        super.setID(id);
        return this;
    }
}
