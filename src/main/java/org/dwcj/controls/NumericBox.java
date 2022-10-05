package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;


/* ==REMOVED FINAL TO ALLOW NumericBoxSpinner TO INHERIT - MH */
public class NumericBox extends AbstractDwcControl implements IReadOnly{

    private BBjInputN numBox;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }

    public NumericBox(String text) {
        setText(text);
    }

    public NumericBox() {
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputN(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            catchUp();
            numBox = (BBjInputN) this.ctrl;
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Returns the comma character used in the NumericBox control.
     * @return Returns the character that is used as the comma in the control.
     */
    public String getCommaCharacter() {
        try {
            return numBox.getCommaCharacter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * Returns the character to be used indicating the decimal point in the NumericBox control.
     * @return Returns the character to be used indicating the decimal point.
     */
    public String getDotCharacter() {
        try {
            return numBox.getDotCharacter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * Returns the EDIT string which is used to map input characters to edit functions for the NumericBox control.
     * @return Returns the EDIT string which is used to map input characters to edit functions for the control.
     */
    public String getEditString() {
        try {
            return numBox.getEditString().toString();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * Returns the number of the last error generated in the NumericBox control.
     * @return Returns the number of the last error generated in the control.
     */
    public int getError() {
        try {
            return numBox.getError();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Returns whether the text in the NumericBox control is highlighted when focus is gained from tabbing into the control.
     * @returnReturns whether the text in the control is highlighted (false = Not highlighted, true = highlighted).
     */
    public boolean isHighlight() {
        try {
            return numBox.getHighlight();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns whether the NumericBox control is in insert mode.
     * @return Returns whether the control is in the insert mode (false = Not in insert mode, true = In insert mode).
     */
    public boolean isInsertMode() {
        try {
            return numBox.getInsertMode();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns the length of the text in the NumericBox control.
     * @return Returns the length of the text in the control.
     */
    public int getLength() {
        try {
            return numBox.getLength();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * This method returns the left margin of the NumericBox control.
     * @return Returns the left margin of the control.
     */
    public int getMargin() {
        try {
            return numBox.getMargin();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Returns the mask of the NumericBox control.
     * @return Returns the mask of the control, which provides character-type verification.
     */
    public String getMask() {
        try {
            return numBox.getMask();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * Returns whether the NumericBox control will accept negative values.
     * @return Returns whether this control will accept negative values.
     */
    public boolean isNegatable() {
        try {
            return numBox.getNegateable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns whether the NumericBox control passes the ENTER key to the top-level window.
     * @return Returns whether the ENTER key is passed to the top-level window (false = Not passed, true = Passed).
     */
    public boolean isPassEnter() {
        try {
            return numBox.getPassEnter();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns whether the NumericBox control passes the TAB key notification to the top-level window.
     * @return Returns whether the TAB key notification is passed to the top-level window (false = Not passed, true = Passed) .
     */
    public boolean isPassTab() {
        try {
            return numBox.getPassTab();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns the restore string of the NumericBox control.
     * @return Returns the restore string of the BBjInputN control - the restore string is the text that appears when a user hits the restore key (usually ESC) or by calling the NumericBox::restore method.
     */
    public String getRestore() {
        try {
            return numBox.getRestore();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return "";
    }

    /**
     * Returns whether comma characters are displayed while in edit mode in the NumericBox control.
     * @return Returns whether comma characters are displayed while in edit mode (0 = Do not display commas, 1 = Display commas). By default, commas are not displayed.
     */
    public boolean isUseEditCommas() {
        try {
            return numBox.getUseEditCommas();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * Returns the value of the contents of the NumericBox control.
     * @return Returns the value of the contents of the control.
     */
    public float getValue() {
        try {
            return numBox.getValue().floatValue();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return -1;
    }

    /**
     * Returns whether the text in the NumericBox control can be edited.
     * @return Returns whether the text in the control can be edited (false = Not Editable, true = Editable). By default, the text is editable.
     */
    @Override
    public Boolean isReadOnly() {
        try {
            return numBox.isEditable();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Sets the text in the NumericBox control to the restore string. The restore string is set in the creation of the BBjInputN control or by calling the NumericBox::setRestoreString method.
     */
    public void restore() {
        try {
            numBox.restore();
        } catch (BBjException e) {
            e.printStackTrace();
        }
    }

    /**
     * This method selects all of the NumericBox control's current contents as if the user highlighted it with the mouse or keyboard.
     * @return Returns this
     */
    public NumericBox selectAll() {
        try {
            numBox.selectAll();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the comma character to be used in the NumericBox control. This will replace the character used to indicate a comma mask character.
     * @param comma - Specifies the character to use for a comma.
     * @return Returns this
     */
    public NumericBox setCommaCharacter(String comma) {
        try {
            numBox.setCommaCharacter(comma);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the character used to indicate the decimal point. This will replace the character used to indicate the decimal point.
     * @param dot - Specifies the character to use, which indicates the decimal point.
     * @return Returns this
     */
    public NumericBox setDotCharacter(String dot) {
        try {
            numBox.setDotCharacter(dot);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the text in the NumericBox control can be edited. By default, the control is editable.
     * @param editable - Sets the editability of the control (false = Not Editable, true = Editable)
     * @return Returns this
     */
    @Override
    public NumericBox setReadOnly(boolean editable) {
        try {
            numBox.setEditable(editable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the EDIT string, which is used to map input characters to edit functions for theNumericBox control.
     * @param edit - Specifies the EDIT string.
     * @return Returns this
     */
    public NumericBox setEditString(String edit) {
        try {
            numBox.setEditString(edit.getBytes());
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether to highlight the text in the control when focus is gained from tabbing into the control.
     * @param highlight - Specifies whether the text is to be highlighted (false = No highlight, true = Highlight)
     * @return Returns this
     */
    public NumericBox setHighlight(boolean highlight) {
        try {
            numBox.setHighlight(highlight);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the NumericBox control is in insert mode.
     * @param insert - Sets whether the control is in insert mode (false = Not in insert mode, true = In insert mode)
     * @return Returns this
     */
    public NumericBox setInsertMode(boolean insert) {
        try {
            numBox.setInsertMode(insert);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the length of the text in the NumericBox control. If the setLength method is called, then the previous mask is not used. The mask for the characters will be the default mask character "X".
     * @param len - Specifies the length of the text in the control.
     * @return Returns this
     */
    public NumericBox setLength(int len) {
        try {
            numBox.setLength(len);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the left margin of the NumericBox control.
     * @param marginWidth - Width of the left margin.
     * @return Returns this
     */
    public NumericBox setMargin(int marginWidth) {
        try {
            numBox.setMargin(marginWidth);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the mask for the NumericBox control.
     * @param mask - Specifies the mask that provides character-type verification.
     * @return Returns this
     */
    public NumericBox setMask(String mask) {
        try {
            numBox.setMask(mask);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the NumericBox control accepts negative values. Negative values are initially accepted.
     * @param negateable - Specifies whether the BBjInputN will accept negative values (false = Negative values are not accepted, true = Negative values are accepted).
     * @return Returns this
     */
    public NumericBox setNegatable(boolean negatable) {
        try {
            numBox.setNegateable(negatable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the NumericBox control is to pass the ENTER key notification to the top-level window. By default, the ENTER key is not passed.
     * @param pass - Specifies whether to pass the ENTER key to the top-level window (false = Not passed, true = Passed).
     * @return Returns this
     */
    public NumericBox setPassEnter(boolean pass) {
        try {
            numBox.setPassEnter(pass);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether the NumericBox control is to pass the TAB key notification to the top-level window.
     * @param pass - Specifies whether to pass the TAB key notification to the top-level window (false = Not passed, true = Passed).
     * @return Returns this
     */
    public NumericBox setPassTab(boolean pass) {
        try {
            numBox.setPassEnter(pass);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the restore value of the NumericBox control. The restore value is the number that appears when the user hits the restore key (usually ESC) or by calling the NumericBox::restore method.
     * @param restore - Specifies the restore value.
     * @return Returns this
     */
    public NumericBox setRestore(float restore) {
        try {
            numBox.setRestore(String.valueOf(restore));
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets whether to display the comma characters in the mask while in edit mode in the NumericBox control.
     * @param useCommas - Specifies whether to display the comma characters in the mask while in edit mode (false = No display, true = Display)
     * @return Returns this
     */
    public NumericBox setUseEditCommas(boolean useCommas) {
        try {
            numBox.setUseEditCommas(useCommas);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    /**
     * Sets the value of the NumericBox control.
     * Note that a mask within a control does NOT round, but a mask within the STR() DOES round. For example, when placing a value such as 12.34567 into an inputN control that is masked with ###0.00, you'll get 12.34. However, if in console mode you print str(12.34567:"###0.00"), you'll get 12.35.
     * @param value - Specifies the value to be set in the control.
     * @return Returns this
     */
    public NumericBox setValue(float value) {
        try {
            numBox.setValue(BasisNumber.createBasisNumber(value));
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public NumericBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public NumericBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public NumericBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public NumericBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

    public NumericBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public NumericBox setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public NumericBox setID(String id){
        super.setID(id);
        return this;
    }
}
