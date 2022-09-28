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
public final class Button extends AbstractDwcControl implements IThemable, IExpansible, IEnable {

    private ButtonPushEventSink buttonPushEventSink;

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
        super.setText(text);
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
     * Clicks the button, for testing purposes
     */
    public void doClick() {
        this.buttonPushEventSink.doClick();
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

    @Override
    public Button setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
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

    @Override
    public Button setTheme(Theme theme) {
        super.setControlTheme(theme);
        return this;
    }

    @Override
    public Button setText(String text) {
        super.setText(text);
        return this;
    }

    @Override
    public Button setEnabled(boolean enabled) {
        if (this.ctrl != null) try {
            ctrl.setEnabled(enabled);
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return this;
    }

    @Override
    public boolean isEnabled() {
        if (this.ctrl != null) try {
            return ctrl.isEnabled();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return true;
    }

}
