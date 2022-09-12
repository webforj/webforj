package org.dwcj.events.sinks.fontchooser;

import com.basis.bbj.proxies.event.BBjFileChooserCancelEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import org.dwcj.Environment;
import org.dwcj.bridge.ControlAccessor;
import org.dwcj.controls.FontChooser;
import org.dwcj.events.fontchooser.FontChooserCancelEvent;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.function.Consumer;

public final class FontChooserCancelEventSink {

    private ArrayList<Consumer<FontChooserCancelEvent>> targets;

    private final FontChooser fontChooser;

    @SuppressWarnings({"static-access"})
    public FontChooserCancelEventSink(FontChooser fc, Consumer<FontChooserCancelEvent> callback) {
        this.targets.add(callback);
        this.fontChooser = fc;

        BBjControl bbjctrl = null;
        try {
            bbjctrl = ControlAccessor.getDefault().getBBjControl(fc);
            bbjctrl.setCallback(Environment.getInstance().getBBjAPI().ON_FILECHOOSER_CANCEL,
                    Environment.getInstance().getDwcjHelper().getEventProxy(this, "cancelEvent"),
                    "onEvent");
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }

    public void changeEvent(BBjFileChooserCancelEvent ev) { //NOSONAR
        FontChooserCancelEvent dwcEv = new FontChooserCancelEvent(this.fontChooser);
        Iterator<Consumer<FontChooserCancelEvent>> it = targets.iterator();
        while (it.hasNext())
            it.next().accept(dwcEv);
    }

    public void addCallback(Consumer<FontChooserCancelEvent> callback) { targets.add(callback); }
}
