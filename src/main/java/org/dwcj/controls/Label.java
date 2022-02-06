package org.dwcj.controls;

import org.dwcj.panels.IPanel;

import com.basis.bbj.proxies.sysgui.BBjWindow;
import com.basis.startup.type.BBjException;

public class Label extends AbstractDwcControl {
	
	private String sText="";
	
	public Label() {
	}
	
	public Label(String text) {
		this.sText = text;
	}
	
	@Override
	public void create(IPanel p) {
		BBjWindow w = p.getBBjWindow();

		try {
			ctrl = w.addStaticText(w.getAvailableControlID(),BASISNUMBER_1,BASISNUMBER_1,BASISNUMBER_1,BASISNUMBER_1,sText);
		} catch (BBjException e) {
			e.printStackTrace();
		}
	
	}






	
}
