package org.dwcj.controls;

import java.util.function.Consumer;

import org.dwcj.events.BBjEventPump;
import org.dwcj.events.ButtonPushEvent;
import org.dwcj.panels.IPanel;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

public class Button extends AbstractDwcControl {
	
	private String sText="";
	
	public Button() {
	}
	
	public Button(String text) {
		this.sText = text;
	}
	
	@Override
	public void create(IPanel p) {
		BBjWindow w = p.getBBjWindow();

		try {
			ctrl = w.addButton(w.getAvailableControlID(),BASISNUMBER_1,BASISNUMBER_1,BASISNUMBER_1,BASISNUMBER_1,sText);
		} catch (BBjException e) {
			e.printStackTrace();
		}
	
	}

	public void onClick(Consumer<ButtonPushEvent> callback) {
		BBjEventPump.setBBjButtonPushCallback(ctrl, callback);
	}





	
}
