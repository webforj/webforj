package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCheckableMenuItem;
import com.basis.startup.type.BBjException;

public class CheckableMenuItem extends MenuItem {

    private BBjCheckableMenuItem bbjCheckableMenuItem;

    public void setSelected(boolean selected) {
        try {
            bbjCheckableMenuItem.setSelected(selected);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isSelected() {
        try {
            return bbjCheckableMenuItem.isSelected();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }
}
