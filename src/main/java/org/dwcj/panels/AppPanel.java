package org.dwcj.panels;

import org.dwcj.Environment;
import org.dwcj.controls.AbstractDwcControl;
import org.dwcj.controls.IStyleable;
import org.dwcj.controls.IThemable;
import org.dwcj.exceptions.DwcAppInitializeException;

import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.google.common.primitives.Ints;

public class AppPanel extends AbstractDwcPanel  {
	
	public AppPanel() throws DwcAppInitializeException {
		
		try {
			BasisNumber b1 	= BasisNumber.createBasisNumber(1);
			BasisNumber ctx = BasisNumber.createBasisNumber(Environment.getInstance().getSysGui().getAvailableContext());
			wnd = Environment.getInstance().getSysGui().addWindow(ctx,b1,b1,b1,b1,"AppPanel", Ints.toByteArray(0x01111088));
		} catch (NumberFormatException | BBjException e) {
			e.printStackTrace();
			throw new DwcAppInitializeException(e);
		} 

	}

	public void add(AbstractDwcControl ctrl) {
		ctrl.create(this);
		
	}

	@Override
	public void setStyle(String property, String value) {
		wnd.setPanelStyle(property, value);
	}
	
}
