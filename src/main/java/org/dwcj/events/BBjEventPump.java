package org.dwcj.events;

import com.basis.bbj.proxies.sysgui.BBjControl;

import java.util.function.Consumer;

public class BBjEventPump {

    public static BBjButtonPushEventSink setBBjButtonPushCallback(BBjControl ctrl, Consumer<ButtonPushEvent> target) {
        return new BBjButtonPushEventSink(ctrl, target);
    }

}
