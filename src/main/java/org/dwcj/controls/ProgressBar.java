package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjProgressBar;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class ProgressBar extends AbstractDwcControl implements IStyleable, IThemable {

    private BBjProgressBar progressBar;

    public ProgressBar() {}

    @Override
    void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addProgressBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
            progressBar = (BBjProgressBar) ctrl;
        } catch (Exception e)  {
            e.printStackTrace();
        }
    }

    public int getMaximum() {
        try {
            return progressBar.getMaximum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getMinimum() {
        try {
            return progressBar.getMinimum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getOrientation() {
        try {
            return progressBar.getOrientation();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public String getText() {
        try {
            return progressBar.getText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    public int getValue() {
        try {
            return progressBar.getValue();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public boolean isIndeterminate() {
        try {
            return progressBar.isIndeterminate();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean isStringPainted() {
        try {
            return progressBar.isStringPainted();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public void setIndeterminate(boolean indeterminate) {
        try {
            progressBar.setIndeterminate(indeterminate);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMaximum(int maximum) {
        try {
            progressBar.setMaximum(maximum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMinimum(int minimum) {
        try {
            progressBar.setMinimum(minimum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setOrientation(int orientation) {
        try {
            progressBar.setOrientation(orientation);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setStringPainted(boolean value) {
        try {
            progressBar.setStringPainted(value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setProgressBarText(String text) {
        try {
            progressBar.setText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setValue(int value) {
        try {
            progressBar.setValue(value);
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
