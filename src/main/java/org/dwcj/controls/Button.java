package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.events.sinks.ButtonPushEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

/**
 * A Push Button
 */
public final class Button extends AbstractDwcControl {

    private ButtonPushEventSink buttonPushEventSink;

    public static enum Expanse{
        LARGE("l"), MEDIUM("m"), SMALL("s"), XLARGE("xl"), XSMALL("xs");

        public final String expanse;

        private Expanse(String expanse){
            this.expanse = expanse;
        }
    }

    public static enum Theme{
        DEFAULT, DANGER, GRAY, INFO, PRIMARY, SUCCESS, WARNING, OUTLINED_DANGER,
        OUTLINED_DEFAULT, OUTLINED_GRAY, OUTLINED_INFO, OUTLINED_SUCCESS, OUTLINED_PRIMARY,
        OUTLINED_WARNING
    }

    public static enum TextVertialAlignment{
        TOP(1), CENTER(0), BOTTOM(3);

        public final Integer alignment;

        private TextVertialAlignment(Integer alignment){
            this.alignment = alignment;
        }
    }

    TextVertialAlignment alignment = TextVertialAlignment.CENTER;

    /**
     * create a Button
     */
    public Button() {
    }

    /**
     * Parameterized button constructor, accepts a string as an argument which will be the initial text displayed on the button
     *
     * @param text String value for initial button text
     * 
     *
     */
    public Button(String text) {
        super.setControlText(text);
    }

    @Override
    protected void create(AbstractDwcjPanel p) {

        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, super.getText());
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * register an event callback for the click event
     *
     * @param callback A method to receive the click event
     * @return the control itself
     */
    public Button onClick(Consumer<ButtonPushEvent> callback) {
        if (this.buttonPushEventSink==null)
            this.buttonPushEventSink = new ButtonPushEventSink(this, callback);
        else this.buttonPushEventSink.addCallback(callback);
        return this;
    }

    /**
     * Accessor for whether or not the button is disabled. 
     * @return Boolean value 
     */
    public boolean isDisableOnClick() {
        //todo: why could an exception be thrown?
        BBjButton btn = (BBjButton) this.ctrl;
        try {
            return btn.getDisableOnClick();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    public TextVertialAlignment getVerticalAlignment(){
        return this.alignment;
    }

    /**
     * Mutator for whether or not the button is disabled on click
     * @param disable Boolean value
     * @return Instance of the object to enable method chaining.
     */
    public Button setDisableOnClick(boolean disable) {
        BBjButton btn = (BBjButton) this.ctrl;
        try {
            btn.setDisableOnClick(disable);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    public Button setVerticalAlignment(TextVertialAlignment alignment){
        BBjButton btn = (BBjButton) this.ctrl;
        if(this.ctrl != null){
            try{
                btn.setVerticalAlignment(alignment.alignment);
            } catch(BBjException e){
                e.printStackTrace();
            }
        }
        return this;
    }














    @Override
    public Button setText(String text) {
        super.setControlText(text);
        return this;
    }

    @Override
    public Button setVisible(Boolean visible){
        super.setControlVisible(visible);
        return this;
    }
    
    @Override
    public Button setEnabled(Boolean enabled) {
        super.setControlEnabled(enabled);
        return this;
    }

    @Override
    public Button setTooltipText(String text) {
        super.setControlTooltipText(text);
        return this;
    }

    @Override
    public Button setAttribute(String attribute, String value){
        super.setControlAttribute(attribute, value);
        return this;
    }

    @Override
    public Button setID(String id){
        super.setControlID(id);
        return this;
    }

    @Override
    public Button setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }
    
    @Override
    public Button addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public Button removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }


    public Button setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    public Button setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }
        
}
