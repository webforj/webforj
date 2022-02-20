package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.events.sinks.BBjButtonPushEventSink;
import org.dwcj.panels.IPanel;

import java.util.function.Consumer;

public class Button extends AbstractDwcControl implements IStyleable, IThemable, IExpansible {

    private String sText = "";

    public Button() {
    }

    public Button(String text) {
        this.sText = text;
    }

    @Override
    public void create(IPanel p) {
        BBjWindow w = p.getBBjWindow();

        try {
            ctrl = w.addButton(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, sText);
        } catch (BBjException e) {
            e.printStackTrace();
        }

    }

    public void onClick(Consumer<ButtonPushEvent> callback) {
        new BBjButtonPushEventSink(this, callback);
    }

    @Override
    public void setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
    }

    @Override
    public void setStyle(String property, String value) {
        super.setControlStyle(property, value);
    }

    @Override
    public void addClass(String selector) {
        super.addControlCssClass(selector);
    }

    @Override
    public void removeClass(String selector) {
        super.removeControlCssClass(selector);
    }

    @Override
    public void setTheme(Theme theme) {
        super.setControlTheme(theme);
    }
}
