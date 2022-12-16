package org.dwcj.controls;

import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.dwcj.interfaces.HasAttribute;
import org.dwcj.interfaces.HasComputedStyle;
import org.dwcj.interfaces.HasControlText;
import org.dwcj.interfaces.HasClassName;
import org.dwcj.interfaces.HasStyle;
import org.dwcj.interfaces.Enableable;
import org.dwcj.interfaces.HasMouseWheelCondition;
import org.dwcj.interfaces.TextAlignable;
import org.dwcj.interfaces.TextHighlightable;
import org.dwcj.interfaces.HasTooltip;
import org.dwcj.interfaces.HasVisibility;




/**
 * The base class for most DWC/BBj controls. Extends the AbstractControl class, and implements
 * default behaviors for the implemented interface methods. 
 */
public abstract class AbstractDwcControl extends AbstractControl implements HasAttribute, HasControlText, HasComputedStyle, HasClassName, HasStyle, Enableable, HasTooltip, HasVisibility{


    /*=====================================================================================
     * Members implemented for interfacing with BBj methods/implementations
     *=====================================================================================
     */
    public static final String STR_EXPANSE = "expanse";
    public static final String STR_THEME = "theme";
    protected static final BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
    protected static final BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
    protected static final BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);

    /*=====================================================================================
     * Members common to all inheriting controls
     *=====================================================================================
     */
    private String text = "";
    private Boolean visible = true;
    private Boolean enabled = true;
    private String tooltipText = "";

    private final Map<String, String> attributes = new HashMap<>();
    private final List<String> cssClasses = new ArrayList<>();
    private final Map<String, String> styles = new HashMap<>();
    

    /*=====================================================================================
     * Theme and Expanse variables which need to be enumerated in their respective child 
     * components
     *=====================================================================================
     */
    private Enum<?> theme = null;
    private Enum<?> expanse = null;


    /*=====================================================================================
     * Interface-controlled members
     *=====================================================================================
     */
    protected Boolean readOnly = null;
    protected Boolean focusable = null;
    protected Boolean tabTraversable = null;
    protected TextAlignable.Alignment textAlignment = null;
    protected Integer horizontalScrollBarPosition = null; 
    protected Integer verticalScrollBarPosition = null;
    protected HasMouseWheelCondition.MouseWheelCondition mouseWheelCondition = null;
    protected TextHighlightable.Highlight textHighlight = null;

    @Override
    public String getAttribute(String attribute) {
        //ask the control first
        if (ctrl != null) try {
            return ctrl.getAttribute(attribute);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        //fall back to the internal list
        return attributes.get(attribute);
    }

    /**
     * set the value for an attribute in the control
     * @param attribute the name of the attribute
     * @param value the value to be set
     * @return the control itself
     */
    @Override
    public AbstractDwcControl setAttribute(String attribute, String value) {
        if (ctrl != null) try {
            ctrl.setAttribute(attribute, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        attributes.put(attribute, value);
        return this;
    }


    @Override
    public String getText() {
        if (ctrl != null) try {
            return ctrl.getText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return text;
    }

    @Override
    public AbstractDwcControl setText(String text) {
        if (ctrl != null){
            try {
                ctrl.setText(text);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        } 
        if (text != null){
            this.text = new String(text.getBytes());
        }
        else{
            this.text = "<null>";
        }
        return this;
    }

    @Override
    public String getComputedStyle(String property){
        if (ctrl != null) try {
            ctrl.getComputedStyle(property);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public AbstractDwcControl addClassName(String selector) {
        if (ctrl != null) try {
            ctrl.addStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.cssClasses.add(selector);
        return this;
    }

    @Override
    public AbstractDwcControl removeClassName(String selector) {
        if (ctrl != null) try {
            ctrl.removeStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.cssClasses.remove(selector);
        return this;
    }

    @Override
    public AbstractDwcControl setStyle(String property, String value) {
        if (ctrl != null) try {
            ctrl.setStyle(property, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.styles.put(property, value);
        return this;
    }


    @Override
    public Boolean isEnabled() {
        if (this.ctrl != null) try {
            return ctrl.isEnabled();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return enabled;
    }

    @Override
    public AbstractDwcControl setEnabled(Boolean enabled) {
        if (this.ctrl != null) try {
            ctrl.setEnabled(enabled);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.enabled = enabled;
        return this;
    }

    @Override
    public String getTooltipText() {
        if (this.ctrl != null) try {
            return ctrl.getToolTipText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return tooltipText;
    }

    @Override
    public AbstractDwcControl setTooltipText(String text) {
        if (this.ctrl != null) try {
            ctrl.setToolTipText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.tooltipText = text;
        return this;
    }

    
    @Override
    public Boolean isVisible() {
        if (this.ctrl != null) try {
            return ctrl.isVisible();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return visible;
    }

    @Override
    public AbstractDwcControl setVisible(Boolean visible) {
        if (this.ctrl != null) try {
            ctrl.setVisible(visible);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.visible = visible;
        return this;
    }






    /*=====================================================================================
     * Implementation to allow child controls to utilize base class Theme and Expanse
     * setters with their own option-appropriate Enums.
     *=====================================================================================
     */

    protected void setControlTheme(Enum<?> theme) {
        if (ctrl != null) try {
            switch (theme.toString()) {
                case "DEFAULT":
                    ctrl.setAttribute(STR_THEME, "default");
                    break;
                case "DANGER":
                    ctrl.setAttribute(STR_THEME, "danger");
                    break;
                case "GRAY":
                    ctrl.setAttribute(STR_THEME, "gray");
                    break;
                case "INFO":
                    ctrl.setAttribute(STR_THEME, "info");
                    break;
                case "PRIMARY":
                    ctrl.setAttribute(STR_THEME, "primary");
                    break;
                case "SUCCESS":
                    ctrl.setAttribute(STR_THEME, "success");
                    break;
                case "WARNING":
                    ctrl.setAttribute(STR_THEME, "warning");
                    break;
                case "OUTLINED_DANGER":
                    ctrl.setAttribute(STR_THEME, "outlined-danger");
                    break;
                case "OUTLINED_DEFAULT":
                    ctrl.setAttribute(STR_THEME, "outlined-default");
                    break;
                case "OUTLINED_GRAY":
                    ctrl.setAttribute(STR_THEME, "outlined-gray");
                    break;
                case "OUTLINED_INFO":
                    ctrl.setAttribute(STR_THEME, "outlined-info");
                    break;
                case "OUTLINED_SUCCESS":
                    ctrl.setAttribute(STR_THEME, "outlined-success");
                    break;
                case "OUTLINED_WARNING":
                    ctrl.setAttribute(STR_THEME, "outlined-warning");
                    break;
                case "OUTLINED_PRIMARY":
                    ctrl.setAttribute(STR_THEME, "outlined-primary");
                    break;
                default:
                    //noop
            }
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.theme = theme;
    }


    protected void setControlExpanse(Enum<?> expanse) {
        if (ctrl != null) try {
            switch (expanse.toString()) {
                case "LARGE":
                    ctrl.setAttribute(STR_EXPANSE, "l");
                    break;
                case "MEDIUM":
                    ctrl.setAttribute(STR_EXPANSE, "m");
                    break;
                case "SMALL":
                    ctrl.setAttribute(STR_EXPANSE, "s");
                    break;
                case "XLARGE":
                    ctrl.setAttribute(STR_EXPANSE, "xl");
                    break;
                case "XSMALL":
                    ctrl.setAttribute(STR_EXPANSE, "xs");
                    break;
                case "XXSMALL":
                    ctrl.setAttribute(STR_EXPANSE, "xxs");
                    break;
                case "XXXSMALL":
                    ctrl.setAttribute(STR_EXPANSE, "xxxs");
                    break;
                default:
                    //noop
            }
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.expanse = expanse;
    }



    

    /**
     * The catchUp method is used to replay attributes and settings that the API user might have
     * added to a control before its creation. A control is not created before it's added
     * to a panel. Anything that is added between instantiation of a control and its addition to a panel
     * has to be recorded and replayed in this method
     *
     * @throws IllegalAccessException - thrown if an attempt is made to call this method more than once
     */
    @SuppressWarnings("java:S3776") // tolerate cognitive complexity for now, it's just a batch list of checks
    protected void catchUp() throws IllegalAccessException {
        if (Boolean.TRUE.equals(this.getCaughtUp())) throw new IllegalAccessException("catchUp cannot be called twice");
        super.catchUp();
        
        if (!this.text.isEmpty()) {
            try {
                ctrl.setText(this.text);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        
        if (!Boolean.TRUE.equals(this.visible)) {
            this.setVisible(this.visible);
        }

        if (!Boolean.TRUE.equals(this.enabled)) {
            this.setEnabled(this.enabled);
        }

        if (!this.tooltipText.isEmpty()){
            try{
                ctrl.setToolTipText(this.tooltipText);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        
        if (!this.attributes.isEmpty()) {
            Iterator<String> it = this.attributes.keySet().iterator();
            while (it.hasNext()) {
                String key = it.next();
                this.setAttribute(key, this.attributes.get(key));
            }
        }

        if (!this.styles.isEmpty()) {
            Iterator<String> it = this.styles.keySet().iterator();
            while (it.hasNext()) {
                String key = it.next();
                this.setStyle(key, this.styles.get(key));
            }
        }

        if (!this.cssClasses.isEmpty()) {
            Iterator<String> it = this.cssClasses.iterator();
            while (it.hasNext()) {
                String cl = it.next();
                this.addClassName(cl);
            }
        }

        if (!this.userData.isEmpty()) {
            Iterator<String> it = this.cssClasses.iterator();
            while (it.hasNext()) {
                String cl = it.next();
                this.setUserData(cl, this.userData.get(cl));
            }
        }

        if (this.theme != null) {
            this.setControlTheme(this.theme);
        }

        if (this.expanse != null) {
            this.setControlExpanse(this.expanse);
        }


    }


}
