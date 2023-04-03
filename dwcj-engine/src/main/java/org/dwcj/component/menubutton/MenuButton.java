package org.dwcj.component.menubutton;

import com.basis.bbj.proxies.sysgui.BBjMenuButton;
import com.basis.bbj.proxies.sysgui.BBjPopupMenu;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.Environment;
import org.dwcj.bridge.WindowAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.popupmenu.PopupMenu;
import org.dwcj.component.window.AbstractWindow;

public final class MenuButton extends AbstractDwcComponent {

    private BBjMenuButton bbjMenuButton;

    @Override
    protected void create(AbstractWindow p) {

        try {
            BBjWindow w = WindowAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addMenuButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250, "");
            catchUp();
            bbjMenuButton = (BBjMenuButton) ctrl;
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public PopupMenu addPopupMenu() {
        try {
            return (PopupMenu) bbjMenuButton.addPopupMenu();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public void clearImageSize() {
        try {
            bbjMenuButton.clearImageSize();
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public boolean isDisableOnClick() {
        try {
            return bbjMenuButton.getDisableOnClick();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return false;
    }

    public PopupMenu getPopupMenu() {
        try {
            return (PopupMenu) bbjMenuButton.getPopupMenu();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public String getImageFile() {
        try {
            return bbjMenuButton.getImageFile();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public boolean isPopupMenuVisible() {
        try {
            return bbjMenuButton.isDropdownMenuVisible();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return false;
    }

    public void removePopupMenu() {
        try {
            bbjMenuButton.removeDropdownMenu();
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void setDisableOnClick(boolean disable) {
        try {
            bbjMenuButton.setDisableOnClick(disable);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void setPopupMenu(PopupMenu popupMenu) {
        try {
            bbjMenuButton.setDropdownMenu((BBjPopupMenu) popupMenu);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void setPopupMenuVisible(boolean visible) {
        try {
            bbjMenuButton.setDropdownMenuVisible(visible);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void setImageFile(String file) {
        try {
            bbjMenuButton.setImageFile(file);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void setImageSize(int height, int width) {
        try {
            bbjMenuButton.setImageSize(new BasisNumber(height), new BasisNumber(width));
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }




    @Override
    public MenuButton setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public MenuButton setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public MenuButton setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public MenuButton setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public MenuButton setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public MenuButton setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public MenuButton setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public MenuButton addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public MenuButton removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }


    
}
