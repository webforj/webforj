package org.dwcj.controls.menuButton;

import com.basis.bbj.proxies.sysgui.BBjMenuButton;
import com.basis.bbj.proxies.sysgui.BBjPopupMenu;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.panels.AbstractDwcjPanel;
import org.dwcj.controls.popupMenu.PopupMenu;

public final class MenuButton extends AbstractDwcControl {

    private BBjMenuButton bbjMenuButton;

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addMenuButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            catchUp();
            bbjMenuButton = (BBjMenuButton) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public PopupMenu addPopupMenu() {
        try {
            return (PopupMenu) bbjMenuButton.addPopupMenu();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void clearImageSize() {
        try {
            bbjMenuButton.clearImageSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isDisableOnClick() {
        try {
            return bbjMenuButton.getDisableOnClick();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public PopupMenu getPopupMenu() {
        try {
            return (PopupMenu) bbjMenuButton.getPopupMenu();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public String getImageFile() {
        try {
            return bbjMenuButton.getImageFile();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public boolean isPopupMenuVisible() {
        try {
            return bbjMenuButton.isDropdownMenuVisible();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void removePopupMenu() {
        try {
            bbjMenuButton.removeDropdownMenu();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setDisableOnClick(boolean disable) {
        try {
            bbjMenuButton.setDisableOnClick(disable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPopupMenu(PopupMenu popupMenu) {
        try {
            bbjMenuButton.setDropdownMenu((BBjPopupMenu) popupMenu);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPopupMenuVisible(boolean visible) {
        try {
            bbjMenuButton.setDropdownMenuVisible(visible);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setImageFile(String file) {
        try {
            bbjMenuButton.setImageFile(file);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setImageSize(int height, int width) {
        try {
            bbjMenuButton.setImageSize(new BasisNumber(height), new BasisNumber(width));
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }




    public MenuButton setText(String text) {
        super.setControlText(text);
        return this;
    }

    public MenuButton setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    public MenuButton setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    public MenuButton setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    public MenuButton setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    public MenuButton setID(String id){
        super.setControlID(id);
        return this;
    }

    public MenuButton setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    public MenuButton addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    public MenuButton removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }


    
}
