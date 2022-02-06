package org.dwcj.events;

import java.util.function.Consumer;

import org.dwcj.App;
import org.dwcj.Environment;

import com.basis.bbj.proxies.event.BBjEvent;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;

public class BBjControlEventSink {
	
	private int eventType;
	private Consumer<IDwcEvent> target;

	public BBjControlEventSink(BBjControl ctrl, int event, Consumer<IDwcEvent> target) {
		this.eventType = event;
		this.target = target;

		try {
			ctrl.setCallback(  eventType, Environment.getInstance().getDwcjHelper().getEventProxy(this, "onEvent"),"onEvent");
		} catch (BBjException e) {
			e.printStackTrace();
		}
	}
	
	public void onEvent(BBjEvent ev) {
		App.consoleLog("onEvent generic");
		target.accept(null);
	}
	
	

}
