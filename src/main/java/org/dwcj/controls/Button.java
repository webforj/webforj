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
public final class Button extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private ButtonPushEventSink buttonPushEventSink;

    /**
     * create a Button
     */
    public Button() {
    }

    /**
     * Create the button with a text
     *
     * @param text The caption of the button
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


    public boolean getDisableOnClick() {
        //todo: why could an exception be thrown?
        BBjButton btn = (BBjButton) this.ctrl;
        try {
            return btn.getDisableOnClick();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

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

}
