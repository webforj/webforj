package org.dwcj.controls;

import com.basis.bbj.proxies.sysgui.BBjCheckBox;
import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;
import org.dwcj.bridge.PanelAccessor;
import org.dwcj.events.CheckBoxCheckEvent;
import org.dwcj.events.sinks.BBjCheckBoxCheckEventSink;
import org.dwcj.panels.AbstractDwcjPanel;

import java.util.function.Consumer;

public final class CheckBox extends AbstractDwcControl implements IStyleable, IExpansible {

    private Consumer<CheckBoxCheckEvent> callback;

    @Override
    protected void create(AbstractDwcjPanel p) {
        try {
            BBjWindow w = PanelAccessor.getDefault().getBBjWindow(p);
            //todo: honor visibility flag, if set before adding the control to the form, so it's created invisibly right away
            ctrl = w.addCheckBox(w.getAvailableControlID(), BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, BASISNUMBER_1, "");
            catchUp();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * register an event callback for a checkOn or checkOff event
     *
     * @param callback A method to receive the onCheck event
     * @return
     */
    public CheckBox onCheck(Consumer<CheckBoxCheckEvent> callback) {
        this.callback = callback;
        new BBjCheckBoxCheckEventSink(this, callback);
        return this;
    }

    public void doCheck() {
        CheckBoxCheckEvent dwcEv = new CheckBoxCheckEvent(this);
        callback.accept(dwcEv);
    }

    public boolean isSelected() {
        //todo: why could an exception be thrown?
        try {
            return ((BBjCheckBox) this.ctrl).isSelected();
        } catch (BBjException e) {
            e.printStackTrace();
        }
        return false;
    }

    @Override
    public CheckBox setExpanse(Expanse expanse) {
        super.setControlExpanse(expanse);
        return this;
    }

    @Override
    public CheckBox setStyle(String property, String value) {
        super.setControlStyle(property, value);
        return this;
    }

    @Override
    public CheckBox addClass(String selector) {
        super.addControlCssClass(selector);
        return this;
    }

    @Override
    public CheckBox removeClass(String selector) {
        super.removeControlCssClass(selector);
        return this;
    }

}
