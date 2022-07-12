package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCheckableMenuItem;
import com.basis.startup.type.BBjException;

public class CheckableMenuItem extends MenuItem {

    private BBjCheckableMenuItem checkableMenuItem;

    public CheckableMenuItem() {}

    public void setSelected(boolean selected) {
        try {
            checkableMenuItem.setSelected(selected);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isSelected() {
        try {
            return checkableMenuItem.isSelected();
        } catch (BBjException e) {
            e.printStackTrace();
            return false;
        }
    }
}
