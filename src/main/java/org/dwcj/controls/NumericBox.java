package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjInputN;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.panels.AbstractDwcjPanel;


/* ==REMOVED FINAL TO ALLOW NumericBoxSpinner TO INHERIT - MH */
public class NumericBox extends AbstractDwcControl implements IReadOnly, IFocusable, ITabTraversable, ITextAlignable, ITextControl{

    private BBjInputN numBox;

    public static enum Expanse{
        LARGE, MEDIUM, SMALL, XLARGE, XSMALL
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING
    }




    private String commaChar = ",";
    private String dotChar = ".";
    private String editString = "";
    private Boolean highlight = false;
    private Boolean insertMode = true;
    private Integer length = 9;
    private Integer margin = 3;
    private String mask = "-########";
    private Boolean negatable = true;
    private Boolean pEnter = false;
    private Boolean pTab = false;
    private float restore = 0;
    private Boolean commas = false;
    private float value = 0;

    





    public NumericBox(String text) {
        setText(text);
        this.readOnly = false;
        this.focusable = true;
        this.tabTraversable = true;
        this.textAlignment = Alignment.LEFT; 
        this.textHighlight = Highlight.HIGHLIGHT_NONE;
    }

    public NumericBox() {
        this("");
    }

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addInputN(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1);
            numBox = (BBjInputN) this.ctrl;
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Returns the comma character used in the NumericBox control.
     * @return Returns the character that is used as the comma in the control.
     */
    public String getCommaCharacter() {
        if(this.ctrl != null){
            try {
                return numBox.getCommaCharacter();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.commaChar;
    }

    /**
     * Returns the character to be used indicating the decimal point in the NumericBox control.
     * @return Returns the character to be used indicating the decimal point.
     */
    public String getDotCharacter() {
        if(this.ctrl != null){
            try {
                return numBox.getDotCharacter();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.dotChar;
    }

    /**
     * Returns the EDIT string which is used to map input characters to edit functions for the NumericBox control.
     * @return Returns the EDIT string which is used to map input characters to edit functions for the control.
     */
    public String getEditString() {
        if(this.ctrl != null){
            try {
                return numBox.getEditString().toString();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.editString;
    }

    /**
     * Returns the number of the last error generated in the NumericBox control.
     * @return Returns the number of the last error generated in the control.
     */
    public Integer getError() {
        if(this.ctrl != null){
            try {
                return numBox.getError();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    /**
     * Returns whether the text in the NumericBox control is highlighted when focus is gained from tabbing into the control.
     * @returnReturns whether the text in the control is highlighted (false = Not highlighted, true = highlighted).
     */
    public Boolean isHighlight() {
        if(this.ctrl != null){
            try {
                return numBox.getHighlight();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.highlight;
    }

    /**
     * Returns whether the NumericBox control is in insert mode.
     * @return Returns whether the control is in the insert mode (false = Not in insert mode, true = In insert mode).
     */
    public Boolean isInsertMode() {
        if(this.ctrl != null){
            try {
                return numBox.getInsertMode();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.insertMode;
    }

    /**
     * Returns the length of the text in the NumericBox control.
     * @return Returns the length of the text in the control.
     */
    public int getLength() {
        if(this.ctrl != null){
            try {
                return numBox.getLength();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.length;
    }

    /**
     * This method returns the left margin of the NumericBox control.
     * @return Returns the left margin of the control.
     */
    public Integer getMargin() {
        if(this.ctrl != null){
            try {
                return numBox.getMargin();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.margin;
    }

    /**
     * Returns the mask of the NumericBox control.
     * @return Returns the mask of the control, which provides character-type verification.
     */
    public String getMask() {
        if(this.ctrl != null){
            try {
                return numBox.getMask();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.mask;
    }

    /**
     * Returns whether the NumericBox control will accept negative values.
     * @return Returns whether this control will accept negative values.
     */
    public Boolean isNegatable() {
        if(this.ctrl != null){
            try {
                return numBox.getNegateable();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.negatable;
    }

    /**
     * Returns whether the NumericBox control passes the ENTER key to the top-level window.
     * @return Returns whether the ENTER key is passed to the top-level window (false = Not passed, true = Passed).
     */
    public Boolean isPassEnter() {
        if(this.ctrl != null){
            try {
                return numBox.getPassEnter();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.pEnter;
    }

    /**
     * Returns whether the NumericBox control passes the TAB key notification to the top-level window.
     * @return Returns whether the TAB key notification is passed to the top-level window (false = Not passed, true = Passed) .
     */
    public Boolean isPassTab() {
        if(this.ctrl != null){
            try {
                return numBox.getPassTab();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.pTab;
    }

    /**
     * Returns the restore string of the NumericBox control.
     * @return Returns the restore string of the BBjInputN control - the restore string is the text that appears when a user hits the restore key (usually ESC) or by calling the NumericBox::restore method.
     */
    public String getRestore() {
        if(this.ctrl != null){
            try {
                return numBox.getRestore();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return String.valueOf(restore);
    }

    /**
     * Returns whether comma characters are displayed while in edit mode in the NumericBox control.
     * @return Returns whether comma characters are displayed while in edit mode (0 = Do not display commas, 1 = Display commas). By default, commas are not displayed.
     */
    public Boolean isUseEditCommas() {
        if(this.ctrl != null){
            try {
                return numBox.getUseEditCommas();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.commas;
    }

    /**
     * Returns the value of the contents of the NumericBox control.
     * @return Returns the value of the contents of the control.
     */
    public float getValue() {
        if(this.ctrl != null){
            try {
                return numBox.getValue().floatValue();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this.value;
    }



    /**
     * Sets the text in the NumericBox control to the restore string. The restore string is set in the creation of the BBjInputN control or by calling the NumericBox::setRestoreString method.
     */
    public void restore() {
        if(this.ctrl != null){
            try {
                numBox.restore();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * This method selects all of the NumericBox control's current contents as if the user highlighted it with the mouse or keyboard.
     * @return Returns this
     */
    public NumericBox selectAll() {
        if(this.ctrl != null){
            try {
                numBox.selectAll();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    /**
     * Sets the comma character to be used in the NumericBox control. This will replace the character used to indicate a comma mask character.
     * @param comma - Specifies the character to use for a comma.
     * @return Returns this
     */
    public NumericBox setCommaCharacter(String comma) {
        if(this.ctrl != null){
            try {
                numBox.setCommaCharacter(comma);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.commaChar = comma;
        return this;
    }

    /**
     * Sets the character used to indicate the decimal point. This will replace the character used to indicate the decimal point.
     * @param dot - Specifies the character to use, which indicates the decimal point.
     * @return Returns this
     */
    public NumericBox setDotCharacter(String dot) {
        if(this.ctrl != null){
            try {
                numBox.setDotCharacter(dot);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.dotChar = dot;
        return this;
    }


   

    /**
     * Sets the EDIT string, which is used to map input characters to edit functions for theNumericBox control.
     * @param edit - Specifies the EDIT string.
     * @return Returns this
     */
    public NumericBox setEditString(String edit) {
        if(this.ctrl != null){
            try {
                numBox.setEditString(edit.getBytes());
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.editString = edit;
        return this;
    }

    /**
     * Sets whether to highlight the text in the control when focus is gained from tabbing into the control.
     * @param highlight - Specifies whether the text is to be highlighted (false = No highlight, true = Highlight)
     * @return Returns this
     */
    public NumericBox setHighlight(Boolean highlight) {
        if(this.ctrl != null){
            try {
                numBox.setHighlight(highlight);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.highlight = highlight;
        return this;
    }

    /**
     * Sets whether the NumericBox control is in insert mode.
     * @param insert - Sets whether the control is in insert mode (false = Not in insert mode, true = In insert mode)
     * @return Returns this
     */
    public NumericBox setInsertMode(Boolean insert) {
        if(this.ctrl != null){
            try {
                numBox.setInsertMode(insert);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.insertMode = insert;
        return this;
    }

    /**
     * Sets the length of the text in the NumericBox control. If the setLength method is called, then the previous mask is not used. The mask for the characters will be the default mask character "X".
     * @param len - Specifies the length of the text in the control.
     * @return Returns this
     */
    public NumericBox setLength(Integer len) {
        if(this.ctrl != null){
            try {
                numBox.setLength(len);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.length = len;
        return this;
    }

    /**
     * Sets the left margin of the NumericBox control.
     * @param marginWidth - Width of the left margin.
     * @return Returns this
     */
    public NumericBox setMargin(Integer marginWidth) {
        if(this.ctrl != null){
            try {
                numBox.setMargin(marginWidth);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.margin = marginWidth;
        return this;
    }

    /**
     * Sets the mask for the NumericBox control.
     * @param mask - Specifies the mask that provides character-type verification.
     * @return Returns this
     */
    public NumericBox setMask(String mask) {
        if(this.ctrl != null){
            try {
                numBox.setMask(mask);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.mask = mask;
        return this;
    }

    /**
     * Sets whether the NumericBox control accepts negative values. Negative values are initially accepted.
     * @param negateable - Specifies whether the BBjInputN will accept negative values (false = Negative values are not accepted, true = Negative values are accepted).
     * @return Returns this
     */
    public NumericBox setNegatable(boolean negatable) {
        if(this.ctrl != null){
            try {
                numBox.setNegateable(negatable);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.negatable = negatable;
        return this;
    }

    /**
     * Sets whether the NumericBox control is to pass the ENTER key notification to the top-level window. By default, the ENTER key is not passed.
     * @param pass - Specifies whether to pass the ENTER key to the top-level window (false = Not passed, true = Passed).
     * @return Returns this
     */
    public NumericBox setPassEnter(Boolean pass) {
        if(this.ctrl != null){
            try {
                numBox.setPassEnter(pass);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.pEnter = pass;
        return this;
    }

    /**
     * Sets whether the NumericBox control is to pass the TAB key notification to the top-level window.
     * @param pass - Specifies whether to pass the TAB key notification to the top-level window (false = Not passed, true = Passed).
     * @return Returns this
     */
    public NumericBox setPassTab(Boolean pass) {
        if(this.ctrl != null){
            try {
                numBox.setPassEnter(pass);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.pTab = pass;
        return this;
    }

    /**
     * Sets the restore value of the NumericBox control. The restore value is the number that appears when the user hits the restore key (usually ESC) or by calling the NumericBox::restore method.
     * @param restore - Specifies the restore value.
     * @return Returns this
     */
    public NumericBox setRestore(float restore) {
        if(this.ctrl != null){
            try {
                numBox.setRestore(String.valueOf(restore));
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.restore = restore;
        return this;
    }

    /**
     * Sets whether to display the comma characters in the mask while in edit mode in the NumericBox control.
     * @param useCommas - Specifies whether to display the comma characters in the mask while in edit mode (false = No display, true = Display)
     * @return Returns this
     */
    public NumericBox setUseEditCommas(boolean useCommas) {
        if(this.ctrl != null){
            try {
                numBox.setUseEditCommas(useCommas);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.commas = useCommas;
        return this;
    }

    /**
     * Sets the value of the NumericBox control.
     * Note that a mask within a control does NOT round, but a mask within the STR() DOES round. For example, when placing a value such as 12.34567 into an inputN control that is masked with ###0.00, you'll get 12.34. However, if in console mode you print str(12.34567:"###0.00"), you'll get 12.35.
     * @param value - Specifies the value to be set in the control.
     * @return Returns this
     */
    public NumericBox setValue(float value) {
        if(this.ctrl != null){
            try {
                numBox.setValue(BasisNumber.createBasisNumber(value));
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        this.value = value;
        return this;
    }




    /**
     * Returns whether the text in the NumericBox control can be edited.
     * @return Returns whether the text in the control can be edited (false = Not Editable, true = Editable). By default, the text is editable.
     */
    @Override
    public Boolean isReadOnly() {
        if(this.ctrl != null){
            try {
                return numBox.isEditable();
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    /**
     * Sets whether the text in the NumericBox control can be edited. By default, the control is editable.
     * @param editable - Sets the editability of the control (false = Not Editable, true = Editable)
     * @return Returns this
     */
    @Override
    public NumericBox setReadOnly(Boolean editable) {
        if(this.ctrl != null){
            try {
                numBox.setEditable(editable);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        return this;
    }

    @Override
    public Boolean isFocusable(){
        if(this.ctrl != null){
            try{
                numBox.isFocusable();
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    @Override
    public NumericBox setFocusable(Boolean focusable){
        if(this.ctrl != null) {
            try{
                numBox.setFocusable(focusable);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.focusable = focusable;
        return this;
    }

    @Override
    public Boolean isTabTraversable(){
        if(this.ctrl != null){
            try{
                numBox.isTabTraversable();
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return true;
    }

    @Override
    public NumericBox setTabTraversable(Boolean traverse){
        if(this.ctrl != null){
            try{
                numBox.setTabTraversable(traverse);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.tabTraversable = traverse;
        return this;
    }

    @Override
    public Highlight getHighlightOnFocus(){
        return this.textHighlight;
    } 

    @Override
    public NumericBox setHighlightOnFocus(Highlight highlight){
        if(this.ctrl != null){
            try{
                numBox.setHighlightOnFocus(highlight.highlight);
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        this.textHighlight = highlight;
        return this;
    }

    @Override
    public Alignment getTextAlignment(){
        if(this.ctrl != null){
            return this.textAlignment;
        }
        return null;
    }

    @Override 
    public NumericBox setTextAlignment(Alignment alignment){
        if(this.ctrl != null){
            try{
                numBox.setAlignment(alignment.textPosition);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        this.textAlignment = alignment;
        return this;
    }





    @Override
    public NumericBox setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public NumericBox setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public NumericBox setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public NumericBox setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public NumericBox setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public NumericBox setID(String id){
        super.setControlID(id);
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




    public NumericBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public NumericBox setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }




    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        super.catchUp();


        if(this.commaChar != ","){
            this.setCommaCharacter(this.commaChar);
        }

        if(this.dotChar != "."){
            this.setDotCharacter(this.dotChar);
        }

        if(this.editString != ""){
            this.setEditString(this.editString);
        }

        if(this.highlight != false){
            this.setHighlight(this.highlight);
        }

        if(this.insertMode != true){
            this.setInsertMode(this.insertMode);
        }

        if(this.length != 9){
            this.setLength(this.length);
        }

        if(this.margin != 3){
            this.setMargin(this.margin);
        }

        if(this.mask != "-########"){
            this.setMask(this.mask);
        }

        if(this.negatable != true){
            this.setNegatable(this.negatable);
        }

        if(this.pEnter != false){
            this.setPassEnter(this.pEnter);
        }

        if(this.pTab != false){
            this.setPassTab(this.pTab);
        }

        if(this.restore != 0){
            this.setRestore(this.restore);
        }

        if(this.commas != false){
            this.setUseEditCommas(this.commas);
        }

        if(this.value != 0){
            this.setValue(this.value);
        }

        

        if(this.readOnly != false){
            this.setReadOnly(this.readOnly);
        }

        if(this.focusable != null){
            this.setFocusable(this.focusable);
        }

        if(this.tabTraversable != true){
            this.setTabTraversable(this.tabTraversable);
        }

        if(this.textAlignment != Alignment.LEFT){
            this.setTextAlignment(this.textAlignment);
        }

        if(this.textHighlight != Highlight.HIGHLIGHT_NONE){
            this.setHighlightOnFocus(this.textHighlight);
        }

    }
}
