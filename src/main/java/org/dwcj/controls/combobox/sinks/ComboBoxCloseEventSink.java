package org.dwcj.controls.combobox.sinks;

import com.basis.bbj.proxies.event.BBjListCloseEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.combobox.ComboBox;
import org.dwcj.controls.combobox.events.ComboBoxCloseEvent;
import com.basis.bbj.proxyif.SysGuiEventConstants;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;


public class ComboBoxCloseEventSink {
    
    private ArrayList<Consumer<ComboBoxCloseEvent>> targets = new ArrayList<>();
    private final ComboBox comboBox;
    private BBjControl bbjctrl;

    @SuppressWarnings({"static-access"})
    public ComboBoxCloseEventSink(ComboBox cb) {
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(SysGuiEventConstants.ON_LIST_CLOSE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "closeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings({"static-access"})
    public ComboBoxCloseEventSink(ComboBox cb, Consumer<ComboBoxCloseEvent> callback) {
        this.targets.add(callback);
        this.comboBox = cb;

        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(cb);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_LIST_CLOSE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "closeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }
    }

    @SuppressWarnings("java.S1172")
    public void closeEvent(BBjListCloseEvent ev) { //NOSONAR
        ComboBoxCloseEvent dwcEv = new ComboBoxCloseEvent(this.comboBox);
        Iterator<Consumer<ComboBoxCloseEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<ComboBoxCloseEvent> callback) { targets.add(callback); }

}
