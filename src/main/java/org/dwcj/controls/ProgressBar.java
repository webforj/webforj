package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjProgressBar;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

public final class ProgressBar extends AbstractDwcControl {

    private BBjProgressBar bbjProgressBar;

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addProgressBar(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            bbjProgressBar = (BBjProgressBar) ctrl;
            catchUp();
        } catch (Exception e)  {
            e.printStackTrace();
        }
    }

    /**
     * This method returns the maximum range of theProgressBar control.
     * @return Returns the maximum range of the progress bar.
     */
    public int getMaximum() {
        try {
            return bbjProgressBar.getMaximum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method returns the minimum range of theProgressBar control.
     * @return Returns the minimum range of the progress bar.
     */
    public int getMinimum() {
        try {
            return bbjProgressBar.getMinimum();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /*
     * ===TODO: Fix/update the return value for the Javadoc notes once
     * confirmation of handling the BBj constants is sorted out
     * ===
     */

    /**
     * This method returns the orientation of the ProgressBar control.
     * @return Returns HORIZONTAL if horizontal, VERTICAL if vertical.
     */
    public int getOrientation() {
        try {
            return bbjProgressBar.getOrientation();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method returns the text of aProgressBar control.
     * @return Returns the text (label) of the progress bar control.
     */
    public String getText() {
        try {
            return bbjProgressBar.getText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * This method returns the current value of a ProgressBar control.
     * @return Returns the current value of the progress bar control.
     */
    public int getValue() {
        try {
            return bbjProgressBar.getValue();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method returns whether the ProgressBar control.
     * @return Returns whether the progress bar is indeterminate (false = specific range, true = indeterminate).
     */
    public boolean isIndeterminate() {
        try {
            return bbjProgressBar.isIndeterminate();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method returns whether the ProgressBar control will display a label (defaults to % complete).
     * @return Returns whether the progress bar will show a label (false = no label, true = label will be displayed).
     */
    public boolean isStringPainted() {
        try {
            System.out.println(bbjProgressBar.isStringPainted());
            return bbjProgressBar.isStringPainted();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * This method sets whether the ProgressBar control is indeterminate. This option is not available on all platforms.
     * @param indeterminate - Sets whether the progress bar is indeterminate (false = Progress bar has a fixed range, which can be retrieved with getMinimum() and getMaximum(), true = Progress bar is indeterminate, indicating that the duration of the task is not yet known.)
     * @return Returns this
     */
    public ProgressBar setIndeterminate(boolean indeterminate) {
        try {
            bbjProgressBar.setIndeterminate(indeterminate);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the maximum range for the ProgressBar control.
     * @param maximum - Specifies the maximum range of the BBjProgressBar control.
     * @return Returns this
     */
    public ProgressBar setMaximum(int maximum) {
        try {
            bbjProgressBar.setMaximum(maximum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the minimum range for the ProgressBar control.
     * @param minimum - Specifies the minimum range of the BBjProgressBar control.
     * @return Returns this
     */
    public ProgressBar setMinimum(int minimum) {
        try {
            bbjProgressBar.setMinimum(minimum);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /*
     * ===TODO: Fix/update the parameter value for the Javadoc notes once
     * confirmation of handling the BBj constants is sorted out
     * ===
     */

    /**
     * This method sets the orientation of the ProgressBar control to HORIZONTAL or VERTICAL.
     * @param orientation - Specifies the orientation as HORIZONTAL or VERTICAL.
     * @return Returns this
     */
    public ProgressBar setOrientation(int orientation) {
        try {
            bbjProgressBar.setOrientation(orientation);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method determines whether the ProgressBar control will show a label.
     * @param stringPainted - Specifies whether the progress bar should display a label (false = Not painted, 1 = Painted)
     * @return Returns this
     */
    public ProgressBar setStringPainted(boolean stringPainted) {
        try {
            bbjProgressBar.setStringPainted(stringPainted);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the text (label) of a ProgressBar control.
     * @param text - Specifies the text to be displayed on the BBjProgressBar. If text is set to "", the progress bar will display percentage complete in the format "XX%".
     * @return Returns this
     */
    public ProgressBar setProgressBarText(String text) {
        try {
            bbjProgressBar.setText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * This method sets the value of a ProgressBar control.
     * @param value - Specifies the value of the control.
     * @return Returns this
     */
    public ProgressBar setValue(int value) {
        try {
            bbjProgressBar.setValue(value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }






    @Override
    public ProgressBar setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public ProgressBar setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public ProgressBar setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public ProgressBar setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public ProgressBar setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public ProgressBar setID(String id){
        super.setControlID(id);
        return this;
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
}
