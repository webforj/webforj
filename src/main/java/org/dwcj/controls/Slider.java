package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjSlider;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.HashMap;
import java.util.Map;

public final class Slider extends AbstractDwcControl {

    private BBjSlider bbjSlider;

    private final boolean horizontal;

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, SUCCESS, WARNING
    }

    public Slider(boolean horizontal) { this.horizontal = horizontal; }

    @Override
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

    /*
     * ==I tested the set method and no inversion happens, but this method does properly return the boolean value
     * if it's been changed== -MH
     */

    /**
     * This method gets the orientation of the ProgressBar control. By default, the minimum value of a vertical slider is at the bottom and the maximum value is at the top. For a horizontal slider, the minimum value is to the left and the maximum value is to the right. The orientation reverses for inverted sliders.
     * @return Returns whether the control orientation is inverted.
     */
    public boolean isInverted() {
        try {
            return bbjSlider.getInverted();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method returns the labels from a ProgressBar control.
     * @return Returns a Java Map<Integer,String> structure, where each Integer key is the slider position of the corresponding String label.
     */
    public Map<Integer,String> getLabels() {
        try {
            return bbjSlider.getLabels();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return new HashMap<>();
    }

    /**
     * This method queries the slider's major tick spacing.
     * @return Returns the slider's major tick spacing.
     */
    public int getMajorTickSpacing() {
        try {
            return bbjSlider.getMajorTickSpacing();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method returns the maximum value of the ProgressBar control.
     * @return Returns the maximum value of the control.
     */
    public int getMaximum() {
        try {
            return bbjSlider.getMaximum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method returns the minimum value of the ProgressBar control.
     * @return Returns the minimum value of the control.
     */
    public int getMinimum() {
        try {
            return bbjSlider.getMinimum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method queries the minor tick spacing of the ProgressBar control.
     * @return Returns the slider's minor tick spacing.
     */
    public int getMinorTickSpacing() {
        try {
            return bbjSlider.getMinorTickSpacing();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method returns the orientation of the ProgressBar control.
     * @return Returns the orientation of the control (false = HORIZONTAL, true = VERTICAL).
     */
    public int getOrientation() {
        return bbjSlider.getOrientation();
    }

    /**
     * This method queries whether to paint labels on the ProgressBar control.
     * @return Returns whether labels are painted on this slider.
     */
    public boolean isPaintLabels() {
        try {
            return bbjSlider.getPaintLabels();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method queries whether to paint ticks on the ProgressBar control.
     * @return Returns whether ticks are painted on this slider.
     */
    public boolean isPaintTicks() {
        try {
            return bbjSlider.getPaintTicks();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method queries whether a ProgressBar control should snap to the nearest tick when the user drags the thumb.
     * @return Returns whether the BBjSlider should snap to the nearest tick when the user drags the thumb.
     */
    public boolean isSnapToTicks() {
        try {
            return bbjSlider.getSnapToTicks();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method returns the current value of the ProgressBar control.
     * @return Returns the current value of the control.
     */
    public int getValue() {
        try {
            return bbjSlider.getValue();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method sets the orientation of the ProgressBar control. By default, the minimum value of a vertical slider is at the bottom and the maximum value is at the top. For a horizontal slider, the minimum value is to the left and the maximum value is to the right. The orientation reverses for inverted sliders.
     * @param inverted - Specifies whether the slider orientation is inverted.
     * @return Returns this
     */
    public Slider setInverted(boolean inverted) {
        try {
            bbjSlider.setInverted(inverted);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the custom labels for a ProgressBar control.
     * @param labels - A Java Map<Integer,String> structure, where the Integer key is the slider position of the corresponding String label.
     * @return Returns this
     */
    public Slider setLabels(Map<Integer,String> labels) {
        try {
            bbjSlider.setLabels(labels);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the major tick spacing for a ProgressBar control.
     * @param prop - Specifies the major tick spacing.
     * @return Returns this
     */
    public Slider setMajorTickSpacing(int tick) {
        try {
            bbjSlider.setMajorTickSpacing(tick);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the maximum value of the ProgressBar control.
     * @param value - Specifies the maximum value.
     * @return Returns this
     */
    public Slider setMaximum(int maximum) {
        try {
            bbjSlider.setMaximum(maximum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the minimum value of the ProgressBar control.
     * @param value - Specifies the minimum value.
     * @return Returns this
     */
    public Slider setMinimum(int minimum) {
        try {
            bbjSlider.setMinimum(minimum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the minor tick spacing of a ProgressBar control.
     * @param tick - Specifies the minor tick spacing.
     * @return Returns this
     */
    public Slider setMinorTickSpacing(int tick) {
        try {
            bbjSlider.setMinorTickSpacing(tick);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets whether labels are painted on a ProgressBar control.
     * @param paint - Specifies whether labels are painted on the
     * @return Returns this
     */
    public Slider setPaintLabels(boolean paint) {
        try {
            bbjSlider.setPaintLabels(paint);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets whether ticks are painted on a ProgressBar control.
     * @param paint - Specifies whether ticks are painted on the control.
     * @return Returns this
     */
    public Slider setPaintTicks(boolean paint) {
        try {
            bbjSlider.setPaintTicks(paint);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets whether a ProgressBar control should snap to the nearest tick when the user drags the thumb.
     * @param snap - Specifies whether the control should snap to the nearest tick when the user drags the thumb.
     * @return Returns this
     */
    public Slider setSnapToTicks(boolean snap) {
        try {
            bbjSlider.setSnapToTicks(snap);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the value of the ProgressBar control.
     * @param value - Specifies the slider value.
     * @return Returns this
     */
    public Slider setValue(int value) {
        try {
            bbjSlider.setValue(value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }



    @Override
    public Slider setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public Slider setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public Slider setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public Slider setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public Slider setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public Slider setID(String id){
        super.setControlID(id);
        return this;
    }

    @Override
    public Slider setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    @Override
    public Slider addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public Slider removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }




    public Slider setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

}
