package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.bbj.proxies.sysgui.TextAlignable;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.*;

/**
 * The base class for all controls and panels
 */
public abstract class AbstractDwcControl implements IControl {


    //Members implemented for interfacing with BBj methods/implementations
    public static final String STR_EXPANSE = "expanse";
    public static final String STR_THEME = "theme";
    protected static final BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
    protected static final BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
    protected static final BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);

    
    
    //Members common to all inheriting controls
    protected BBjControl ctrl;
    private String text = "";
    private Boolean visible = null;
    private Boolean enabled = null;
    private String tooltipText = "";
    private final Map<String, String> attributes = new HashMap<>();
    private final Map<String, String> styles = new HashMap<>();
    private final List<String> cssClasses = new ArrayList<>();
    private final Map<String, Object> userData = new HashMap<>();
    
    //Theme and Expanse variables which need to be enumerated in their respective child components
    private Enum<?> theme;
    private Enum<?> expanse;

    //Interface-controlled members
    protected Boolean readOnly = null;
    protected Boolean focusable = null;
    protected Boolean tabTraversable = null;
    protected TextAlignable textAlignment = null;

        
    static {
        ControlAccessor.setDefault(new CtrlAccessorImpl());
    }


    protected boolean caughtUp = false;


    /**
     * Create the object on a panel p. The preferred way of creating an object is using the
     * Panel::add(Control) method, instead of this
     * @param p
     */
    protected void create(AbstractDwcjPanel p) {}

        /**
     * This method returns the underlying original BBj control
     * It's package private and can only be accessed through the ControlAccessor
     * No API user / customer shall ever work directly with BBj controls
     *
     * @return the underlying BBj control
     */
    BBjControl getControl() {
        return this.ctrl;
    }



    /*
    ------------------------------------------------------------
    internal protected methods that implement internal behaviour
    ------------------------------------------------------------
     */

    /* Created abstract methods for abstract class setter methods to be
     * overridden in child classes to enable method chaining
     */
    public abstract AbstractDwcControl setText(String text);
    public abstract AbstractDwcControl setVisible(Boolean visible);
    public abstract AbstractDwcControl setEnabled(Boolean enabled);
    public abstract AbstractDwcControl setTooltipText(String text);
    public abstract AbstractDwcControl setAttribute(String attribute, String value);
    public abstract AbstractDwcControl setID(String id);
    public abstract AbstractDwcControl setStyle(String property, String value);
    public abstract AbstractDwcControl addClass(String selector);
    public abstract AbstractDwcControl removeClass(String selector);

    

    @Override
    public String getText() {
        if (ctrl != null) try {
            return ctrl.getText();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return text;
    }

    protected IControl setControlText(String text) {
        if (ctrl != null) try {
            ctrl.setText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        if (text != null)
            this.text = new String(text.getBytes());
        else
            this.text = "<null>";
        return this;
    }
   
    @Override
    public boolean isVisible() {
        if (this.ctrl != null) try {
            return ctrl.isVisible();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return visible;
    }

    protected IControl setControlVisible(Boolean visible) {
        if (this.ctrl != null) try {
            ctrl.setVisible(visible);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.visible = visible;
        return this;
    }



    @Override
    public boolean isEnabled() {
        if (this.ctrl != null) try {
            return ctrl.isEnabled();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return enabled;
    }

    
    protected IControl setControlEnabled(Boolean enabled) {
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

    protected IControl setControlTooltipText(String text) {
        if (this.ctrl != null) try {
            ctrl.setToolTipText(text);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.tooltipText = text;
        return this;
    }
    
    /**
     * get an attribute of the control
     * @param attribute the name of the attribute
     * @return the value of the attribute in the control
     */
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
    protected IControl setControlAttribute(String attribute, String value) {
        if (ctrl != null) try {
            ctrl.setAttribute(attribute, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        attributes.put(attribute, value);
        return this;
    }

    @Override
    public String getID(){
        if(this.attributes.containsKey("id")){
            return getAttribute("id");
        }
        if(this.ctrl != null){
            try{
                return ctrl.getAttribute("id");
            } catch (BBjException e){
                e.printStackTrace();
            }
        }
        return null;
    }

    protected IControl setControlID(String id){
        this.setAttribute("id", id);
        return this;
    }

    protected void setControlStyle(String property, String value) {
        if (ctrl != null) try {
            ctrl.setStyle(property, value);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.styles.put(property, value);
    }


    protected void addControlCssClass(String selector) {
        if (ctrl != null) try {
            ctrl.addStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.cssClasses.add(selector);
    }


    protected void removeControlCssClass(String selector) {
        if (ctrl != null) try {
            ctrl.removeStyle(selector);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        this.cssClasses.remove(selector);
    }

    public String getComputedStyle(String property){
        if (ctrl != null) try {
            ctrl.getComputedStyle(property);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return null;
    }


    public Object getUserData(String key){
        return userData.get(key);
    }

    public void setUserData(String key, Object data){
        userData.put(key, data);
    }










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
        if (caughtUp) throw new IllegalAccessException("catchUp cannot be called twice");

        if (!this.text.isEmpty()) {
            try {
                ctrl.setText(this.text);
            } catch (BBjException e) {
                e.printStackTrace();
            }
        }
        
        if (this.visible != null) {
            this.setVisible(this.visible);
        }

        if (this.enabled != null) {
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
                this.setControlStyle(key, this.styles.get(key));
            }
        }

        if (!this.cssClasses.isEmpty()) {
            Iterator<String> it = this.cssClasses.iterator();
            while (it.hasNext()) {
                String cl = it.next();
                this.addControlCssClass(cl);
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

        this.caughtUp = true;

    }


}
