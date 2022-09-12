package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjProgressBar;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class ProgressBar extends AbstractDwcControl implements IStyleable, IThemable {

    private BBjProgressBar bbjProgressBar;

    @Override
    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addProgressBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
            bbjProgressBar = (BBjProgressBar) ctrl;
        } catch (Exception e)  {
            e.printStackTrace();
        }
    }

    public int getMaximum() {
        try {
            return bbjProgressBar.getMaximum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getMinimum() {
        try {
            return bbjProgressBar.getMinimum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getOrientation() {
        try {
            return bbjProgressBar.getOrientation();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getText() {
        try {
            return bbjProgressBar.getText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public int getValue() {
        try {
            return bbjProgressBar.getValue();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean isIndeterminate() {
        try {
            return bbjProgressBar.isIndeterminate();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean isStringPainted() {
        try {
            return bbjProgressBar.isStringPainted();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void setIndeterminate(boolean indeterminate) {
        try {
            bbjProgressBar.setIndeterminate(indeterminate);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMaximum(int maximum) {
        try {
            bbjProgressBar.setMaximum(maximum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMinimum(int minimum) {
        try {
            bbjProgressBar.setMinimum(minimum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setOrientation(int orientation) {
        try {
            bbjProgressBar.setOrientation(orientation);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setStringPainted(boolean value) {
        try {
            bbjProgressBar.setStringPainted(value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setProgressBarText(String text) {
        try {
            bbjProgressBar.setText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setValue(int value) {
        try {
            bbjProgressBar.setValue(value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public ProgressBar setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public ProgressBar addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public ProgressBar removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    @Override
    public ProgressBar setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
