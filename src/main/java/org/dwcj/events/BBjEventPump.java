package org.dwcj.events;

import java.util.function.Consumer;
import com.basis.bbj.proxies.sysgui.BBjControl;

public class BBjEventPump {

	public static BBjButtonPushEventSink setBBjButtonPushCallback(BBjControl ctrl, Consumer<ButtonPushEvent> target) {
			return new BBjButtonPushEventSink(ctrl, target);
	}
	
}
