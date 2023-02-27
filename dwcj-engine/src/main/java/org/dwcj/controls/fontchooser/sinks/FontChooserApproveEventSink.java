package org.dwcj.controls.fontchooser.sinks;

import com.basis.bbj.proxies.event.BBjFileChooserApproveEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.fontchooser.FontChooser;
import org.dwcj.controls.fontchooser.events.FontChooserApproveEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class FontChooserApproveEventSink {

    private ArrayList<Consumer<FontChooserApproveEvent>> targets;

    private final FontChooser fontChooser;

    @SuppressWarnings({"static-access"})
    public FontChooserApproveEventSink(FontChooser fc, Consumer<FontChooserApproveEvent> callback) {
        this.targets.add(callback);
        this.fontChooser = fc;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(fc);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_FILECHOOSER_APPROVE,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "changeEvent"),
                    "onEvent");
        } catch (Exception e) {
            Environment.logError(e);
        }

    }

    public void changeEvent(BBjFileChooserApproveEvent ev) { // NOSONAR
        FontChooserApproveEvent dwcEv = new FontChooserApproveEvent(this.fontChooser);
        Iterator<Consumer<FontChooserApproveEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<FontChooserApproveEvent> callback) { targets.add(callback); }
}
