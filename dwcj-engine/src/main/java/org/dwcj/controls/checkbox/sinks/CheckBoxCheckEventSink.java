package org.dwcj.controls.checkbox.sinks;

import com.basis.bbj.proxies.event.BBjCheckOffEvent;
import com.basis.bbj.proxies.event.BBjCheckOnEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.checkbox.CheckBox;
import org.dwcj.controls.checkbox.events.CheckBoxChangeEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class CheckBoxCheckEventSink {

    private final ArrayList<Consumer<CheckBoxChangeEvent>> targets = new ArrayList<>();

    private final CheckBox checkBox;

    private BBjControl bbjctrl;

    private static final String ON_EVENT = "onEvent";

    @SuppressWarnings({"static-access"})
    public CheckBoxCheckEventSink(CheckBox cb) {
        this.checkBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_OFF, //NOSONAR
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOffEvent"),
                    ON_EVENT);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_ON, //NOSONAR
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOnEvent"),
                    ON_EVENT);
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public CheckBoxCheckEventSink(CheckBox cb, Consumer<CheckBoxChangeEvent> target) {
        this.targets.add(target);
        this.checkBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_OFF, //NOSONAR
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOffEvent"),
                    ON_EVENT);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_CHECK_ON, //NOSONAR
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "checkOnEvent"),
                    ON_EVENT);
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java:S1172")
    public void checkOffEvent(BBjCheckOffEvent ev) { //NOSONAR
        CheckBoxChangeEvent dwcEv = new CheckBoxChangeEvent(this.checkBox, false);
        Iterator<Consumer<CheckBoxChangeEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    @SuppressWarnings("java:S1172")
    public void checkOnEvent(BBjCheckOnEvent ev) { //NOSONAR
        CheckBoxChangeEvent dwcEv = new CheckBoxChangeEvent(this.checkBox, true);
        Iterator<Consumer<CheckBoxChangeEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<CheckBoxChangeEvent> callback) {
        targets.add(callback);
    }

}
