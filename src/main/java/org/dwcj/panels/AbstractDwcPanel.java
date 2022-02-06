package org.dwcj.panels;


import org.dwcj.controls.IStyleable;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

public abstract class AbstractDwcPanel implements IPanel, IStyleable{

	protected BBjWindow wnd;

	@Override
	public BBjWindow getBBjWindow() {
		return wnd;
	}
	
	@Override
	public void setStyle(String property, String value) {
		try {
			wnd.setStyle(property, value);
		} catch (BBjException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void addClass(String selector) {
		try {
			wnd.addStyle(selector);
		} catch (BBjException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void removeClass(String selector) {
		try {
			wnd.removeStyle(selector);
		} catch (BBjException e) {
			e.printStackTrace();
		}
	}


	
}
