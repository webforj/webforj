package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.BBjMenuItem;
import com.basis.bbj.proxies.sysgui.BBjPopupMenu;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.resource.Font;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.Map;

public class PopupMenu extends AbstractDwcControl implements IStyleable, IPopupAble {

    private BBjPopupMenu popupMenu;

    public PopupMenu() {}

    void create(AbstractDwcjPanel p) {
        try{
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            ctrl = (BBjControl) w.addPopupMenu();
            catchUp();
            popupMenu = (BBjPopupMenu) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public CheckableMenuItem addCheckableMenuItem(int ID, String title) {
        try {
            return (CheckableMenuItem) popupMenu.addCheckableMenuItem(ID, title);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public CheckableMenuItem addCheckableMenuItem(int ID, String title, boolean checked) {
        try {
            return (CheckableMenuItem) popupMenu.addCheckableMenuItem(ID, title, checked);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem addMenuItem(int ID, String title) {
        try {
            return (MenuItem) popupMenu.addMenuItem(ID, title);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem addMenuItem(int ID, String title, int action) {
        try {
            return (MenuItem) popupMenu.addMenuItem(ID, title, action);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem addMenuItem(int ID, String title, boolean checkable, boolean checked) {
        try {
            return (MenuItem) popupMenu.addMenuItem(ID, title, checkable, checked);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem addMenuItem(int ID, String title, boolean checkable, boolean checked, int action) {
        try {
            return (MenuItem) popupMenu.addMenuItem(ID, title, checkable, checked, action);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void addSeparator() {
        try {
            popupMenu.addSeparator();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public CheckableMenuItem getCheckableMenuItem(int ID) {
        try {
            return (CheckableMenuItem) popupMenu.getCheckableMenuItem(ID);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public CheckableMenuItem getCheckableMenuItemAt(int index) {
        try {
            return (CheckableMenuItem) popupMenu.getCheckableMenuItemAt(index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public int getChildCount() {
        return popupMenu.getChildCount();
    }

    public String getClientProperty(Object key) {
        try {
            return popupMenu.getClientProperty(key).toString();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public BBjControl getControl(int ID) {
        try {
            return popupMenu.getControl(ID);
        } catch (BBjException e) {
            e.printStackTrace();
            return null;
        }
    }

    public int getID() {
        return popupMenu.getID();
    }

    public int getMaximumRowCount() {
        return popupMenu.getMaximumRowCount();
    }

    public MenuItem getMenuItem(int ID) {
        try {
            return (MenuItem) popupMenu.getMenuItem(ID);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem getMenuItemAt(int index) {
        try {
            return (MenuItem) popupMenu.getMenuItemAt(index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public int getMenuItemIDAt(int index) {
        try {
            return popupMenu.getMenuItemIDAt(index);
        } catch (BBjException e) {
            e.printStackTrace();
            return -1;
        }
    }

    public String getName() {
        try {
            return popupMenu.getName();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public Object getUserData() {
        try {
            return popupMenu.getUserData();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void hide() {
        try {
            popupMenu.hide();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public CheckableMenuItem insertCheckableMenuItem(int index, int ID, String title) {
        try {
            return (CheckableMenuItem) popupMenu.insertCheckableMenuItem(index, ID, title);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public CheckableMenuItem insertCheckableMenuItem(int index, int ID, String title, boolean checked) {
        try {
            return (CheckableMenuItem) popupMenu.insertCheckableMenuItem(index, ID, title, checked);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem insertMenuItem(int index, int ID, String title) {
        try {
            return (MenuItem) popupMenu.insertMenuItem(index, ID, title);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem insertMenuItem(int index, int ID, String title, int action) {
        try {
            return (MenuItem) popupMenu.insertMenuItem(index, ID, title, action);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem insertMenuItem(int index, int ID, String title, boolean checkable, boolean checked) {
        try {
            return (MenuItem) popupMenu.insertMenuItem(index, ID, title, checkable, checked);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public MenuItem insertMenuItem(int index, int ID, String title, boolean checkable, boolean checked, int action) {
        try {
            return (MenuItem) popupMenu.insertMenuItem(index, ID, title, checkable, checked, action);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void insertSeparator(int index) {
        try {
            popupMenu.insertSeparator(index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public boolean isVisible() {
        try {
            return popupMenu.isVisible();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void putClientProperty(Object key, Object value) {
        try {
            popupMenu.putClientProperty(key, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeMenuItem(MenuItem item) {
        try {
            popupMenu.removeMenuItem((BBjMenuItem) item);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeMenuItem(int ID) {
        try {
            popupMenu.removeMenuItem(ID);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeMenuItemAt(int index) {
        try {
            popupMenu.removeMenuItemAt(index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void removeSeparator(int index) {
        try {
            popupMenu.removeSeparator(index);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMaximumRowCount(int max) {
        popupMenu.setMaximumRowCount(max);
    }

    public void setName(String name) {
        try {
            popupMenu.setName(name);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setUserData(Object object) {
        try {
            popupMenu.setUserData(object);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    //todo: how to handle BBjControl parameter?
    public void show (BBjControl control, int x, int y) {
        try {
            popupMenu.show(control, x, y);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void show(int context, int ID, int x, int y) {
        try {
            popupMenu.show(context, ID, x, y);
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

    @Override
    public IPopupAble addPopupMenu(int id, String title) {
        return null;
    }

    @Override
    public IPopupAble removePopupMenu(int id, String title) {
        return null;
    }
}
