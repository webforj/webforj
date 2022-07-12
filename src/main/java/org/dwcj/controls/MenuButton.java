package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjListButton;
import com.basis.bbj.proxies.sysgui.BBjMenuButton;
import com.basis.bbj.proxies.sysgui.BBjPopupMenu;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjNumber;
import com.basis.util.common.BasisNumber;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class MenuButton extends AbstractDwcControl implements IStyleable {

    private BBjMenuButton menuButton;

    public MenuButton() {}

    void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addMenuButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            catchUp();
            menuButton = (BBjMenuButton) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public PopupMenu addPopupMenu() {
        try {
            return (PopupMenu) menuButton.addPopupMenu();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void clearImageSize() {
        try {
            menuButton.clearImageSize();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean getDisableOnClick() {
        try {
            return menuButton.getDisableOnClick();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public PopupMenu getPopupMenu() {
        try {
            return (PopupMenu) menuButton.getPopupMenu();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public String getImageFile() {
        try {
            return menuButton.getImageFile();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public boolean isPopupMenuVisible() {
        try {
            return menuButton.isDropdownMenuVisible();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void removePopupMenu() {
        try {
            menuButton.removeDropdownMenu();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setDisableOnClick(boolean disable) {
        try {
            menuButton.setDisableOnClick(disable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPopupMenu(PopupMenu popupMenu) {
        try {
            menuButton.setDropdownMenu((BBjPopupMenu) popupMenu);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPopupMenuVisible(boolean visible) {
        try {
            menuButton.setDropdownMenuVisible(visible);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    //todo, wie wollen wir BBjImage translaten?
    public void setImage() {}

    public void setImageFile(String file) {
        try {
            menuButton.setImageFile(file);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setImageSize(int height, int width) {
        try {
            menuButton.setImageSize(new BasisNumber(height), new BasisNumber(width));
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public IStyleable setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public IStyleable addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public IStyleable removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }
}
