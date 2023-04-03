package org.dwcj.component.popupmenu;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjMenuItem;
import com.basis.bbj.proxies.sysgui.BBjPopupMenu;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.Environment;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.component.AbstractDwcComponent;
import org.dwcj.component.HasPopupMenu;
import org.dwcj.component.checkablemenuitem.CheckableMenuItem;
import org.dwcj.component.menuitem.MenuItem;
import org.dwcj.component.window.AbstractWindow;

public class PopupMenu extends AbstractDwcComponent implements HasPopupMenu {

    private BBjPopupMenu bbjPopupMenu;

    @Override
    protected void create(AbstractWindow p) {
        try{
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = (BBjControl) w.addPopupMenu();
            catchUp();
            bbjPopupMenu = (BBjPopupMenu) ctrl;
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    public CheckableMenuItem addCheckableMenuItem(int id, String title) {
        try {
            return (CheckableMenuItem) bbjPopupMenu.addCheckableMenuItem(id, title);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public CheckableMenuItem addCheckableMenuItem(int id, String title, boolean checked) {
        try {
            return (CheckableMenuItem) bbjPopupMenu.addCheckableMenuItem(id, title, checked);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem addMenuItem(int id, String title) {
        try {
            return (MenuItem) bbjPopupMenu.addMenuItem(id, title);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem addMenuItem(int id, String title, int action) {
        try {
            return (MenuItem) bbjPopupMenu.addMenuItem(id, title, action);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem addMenuItem(int id, String title, boolean checkable, boolean checked) {
        try {
            return (MenuItem) bbjPopupMenu.addMenuItem(id, title, checkable, checked);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem addMenuItem(int id, String title, boolean checkable, boolean checked, int action) {
        try {
            return (MenuItem) bbjPopupMenu.addMenuItem(id, title, checkable, checked, action);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public void addSeparator() {
        try {
            bbjPopupMenu.addSeparator();
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public CheckableMenuItem getCheckableMenuItem(int id) {
        try {
            return (CheckableMenuItem) bbjPopupMenu.getCheckableMenuItem(id);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public CheckableMenuItem getCheckableMenuItemAt(int index) {
        try {
            return (CheckableMenuItem) bbjPopupMenu.getCheckableMenuItemAt(index);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public int getChildCount() {
        return bbjPopupMenu.getChildCount();
    }

    public String getClientProperty(Object key) {
        try {
            return bbjPopupMenu.getClientProperty(key).toString();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return "";
    }

    public BBjControl getControl(int id) {
        try {
            return bbjPopupMenu.getControl(id);
        } catch (BBjException e) {
            Environment.logError(e);
            return null;
        }
    }

    public int getMenuID() {
        return bbjPopupMenu.getID();
    }

    public int getMaximumRowCount() {
        return bbjPopupMenu.getMaximumRowCount();
    }

    public MenuItem getMenuItem(int id) {
        try {
            return (MenuItem) bbjPopupMenu.getMenuItem(id);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem getMenuItemAt(int index) {
        try {
            return (MenuItem) bbjPopupMenu.getMenuItemAt(index);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public int getMenuItemIDAt(int index) {
        try {
            return bbjPopupMenu.getMenuItemIDAt(index);
        } catch (BBjException e) {
            Environment.logError(e);
            return -1;
        }
    }

    public String getName() {
        try {
            return bbjPopupMenu.getName();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public Object getUserData() {
        try {
            return bbjPopupMenu.getUserData();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public void hide() {
        try {
            bbjPopupMenu.hide();
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public CheckableMenuItem insertCheckableMenuItem(int index, int id, String title) {
        try {
            return (CheckableMenuItem) bbjPopupMenu.insertCheckableMenuItem(index, id, title);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public CheckableMenuItem insertCheckableMenuItem(int index, int id, String title, boolean checked) {
        try {
            return (CheckableMenuItem) bbjPopupMenu.insertCheckableMenuItem(index, id, title, checked);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem insertMenuItem(int index, int id, String title) {
        try {
            return (MenuItem) bbjPopupMenu.insertMenuItem(index, id, title);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem insertMenuItem(int index, int id, String title, int action) {
        try {
            return (MenuItem) bbjPopupMenu.insertMenuItem(index, id, title, action);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem insertMenuItem(int index, int id, String title, boolean checkable, boolean checked) {
        try {
            return (MenuItem) bbjPopupMenu.insertMenuItem(index, id, title, checkable, checked);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public MenuItem insertMenuItem(int index, int id, String title, boolean checkable, boolean checked, int action) {
        try {
            return (MenuItem) bbjPopupMenu.insertMenuItem(index, id, title, checkable, checked, action);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return null;
    }

    public void insertSeparator(int index) {
        try {
            bbjPopupMenu.insertSeparator(index);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    @Override
    public Boolean isVisible() {
        try {
            return bbjPopupMenu.isVisible();
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return false;
    }

    public void putClientProperty(Object key, Object value) {
        try {
            bbjPopupMenu.putClientProperty(key, value);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void removeMenuItem(MenuItem item) {
        try {
            bbjPopupMenu.removeMenuItem((BBjMenuItem) item);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void removeMenuItem(int id) {
        try {
            bbjPopupMenu.removeMenuItem(id);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void removeMenuItemAt(int index) {
        try {
            bbjPopupMenu.removeMenuItemAt(index);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void removeSeparator(int index) {
        try {
            bbjPopupMenu.removeSeparator(index);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public PopupMenu setMaximumRowCount(int max) {
        bbjPopupMenu.setMaximumRowCount(max);
        return this;
    }

    public PopupMenu setName(String name) {
        try {
            bbjPopupMenu.setName(name);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return this;
    }

    public PopupMenu setUserData(Object object) {
        try {
            bbjPopupMenu.setUserData(object);
        } catch (BBjException e) {
            Environment.logError(e);
        }
        return this;
    }

    //todo: how to handle BBjControl parameter?
    public void show (BBjControl control, int x, int y) {
        try {
            bbjPopupMenu.show(control, x, y);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }

    public void show(int context, int id, int x, int y) {
        try {
            bbjPopupMenu.show(context, id, x, y);
        } catch (BBjException e) {
            Environment.logError(e);
        }
    }


    @Override
    public HasPopupMenu addPopupMenu(int id, String title) {
        return null;
    }

    @Override
    public HasPopupMenu removePopupMenu(int id, String title) {
        return null;
    }





    @Override
    public PopupMenu setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public PopupMenu setVisible(Boolean visible){
        super.setVisible(visible);
        return this;
    }
    
    @Override
    public PopupMenu setEnabled(Boolean enabled) {
        super.setEnabled(enabled);
        return this;
    }

    @Override
    public PopupMenu setTooltipText(String text) {
        super.setTooltipText(text);
        return this;
    }

    @Override
    public PopupMenu setAttribute(String attribute, String value){
        super.setAttribute(attribute, value);
        return this;
    }

    @Override
    public PopupMenu setId(String elementId){
        super.setId(elementId);
        return this;
    }

    @Override
    public PopupMenu setStyle(String property, String value) {
        super.setStyle(property, value);
        return this;
    }
    
    @Override
    public PopupMenu addClassName(String selector) {
        super.addClassName(selector);
        return this;
    }

    @Override
    public PopupMenu removeClassName(String selector) {
        super.removeClassName(selector);
        return this;
    }
}
