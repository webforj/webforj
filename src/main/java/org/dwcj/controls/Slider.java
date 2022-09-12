package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjSlider;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.HashMap;
import java.util.Map;

public final class Slider extends AbstractDwcControl implements IControl, IStyleable, IThemable {

    private BBjSlider bbjSlider;

    private final boolean horizontal;

    public Slider(boolean horizontal) { this.horizontal = horizontal; }

    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            if (horizontal)
                ctrl = w.addHorizontalSlider(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250);
            else
                ctrl = w.addVerticalSlider(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_250, BASISNUMBER_250);
            catchUp();
            bbjSlider = (BBjSlider) ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public boolean getInverted() {
        try {
            return bbjSlider.getInverted();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public Map<Integer,String> getLabels() {
        try {
            return bbjSlider.getLabels();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new HashMap<>();
    }

    public int getMajorTickSpacing() {
        try {
            return bbjSlider.getMajorTickSpacing();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getMaximum() {
        try {
            return bbjSlider.getMaximum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getMinimum() {
        try {
            return bbjSlider.getMinimum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getMinorTickSpacing() {
        try {
            return bbjSlider.getMinorTickSpacing();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public int getOrientation() {
        return bbjSlider.getOrientation();
    }

    public boolean getPaintLabels() {
        try {
            return bbjSlider.getPaintLabels();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getPaintTicks() {
        try {
            return bbjSlider.getPaintTicks();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public boolean getSnapToTicks() {
        try {
            return bbjSlider.getSnapToTicks();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public int getValue() {
        try {
            return bbjSlider.getValue();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    public void setInverted(boolean inverted) {
        try {
            bbjSlider.setInverted(inverted);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setLabels(Map<Integer,String> labels) {
        try {
            bbjSlider.setLabels(labels);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMajorTickSpacing(int tick) {
        try {
            bbjSlider.setMajorTickSpacing(tick);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMaximum(int maximum) {
        try {
            bbjSlider.setMaximum(maximum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMinimum(int minimum) {
        try {
            bbjSlider.setMinimum(minimum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setMinorTickSpacing(int tick) {
        try {
            bbjSlider.setMinorTickSpacing(tick);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPaintLabels(boolean paint) {
        try {
            bbjSlider.setPaintLabels(paint);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setPaintTicks(boolean paint) {
        try {
            bbjSlider.setPaintTicks(paint);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setSnapToTicks(boolean snap) {
        try {
            bbjSlider.setSnapToTicks(snap);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    public void setValue(int value) {
        try {
            bbjSlider.setValue(value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    @Override
    public IStyleable setStyle(String property, String value) {
        super.setControlStyle(property,value);
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
    public IThemable setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
}
