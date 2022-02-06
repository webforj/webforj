package org.dwcj.controls;

import org.dwcj.exceptions.DwcInvalidStyleError;
import org.dwcj.panels.IPanel;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.basis.util.common.BasisNumber;
import com.basis.util.common.BasisNumber;

public abstract class AbstractDwcControl implements IThemable, IStyleable{
	
	
	public final static int THEME_DEFAULT 	= 100;
	public final static int THEME_DANGER 	= 101;
	public final static int THEME_GRAY 		= 102;
	public final static int THEME_INFO 		= 103;
	public final static int THEME_PRIMARY 	= 104;
	public final static int THEME_SUCCESS 	= 105;
	public final static int THEME_WARNING 	= 106;
	
	public final static int EXPANSE_LARGE 	= 200;
	public final static int EXPANSE_MEDIUM 	= 201;
	public final static int EXPANSE_SMALL 	= 202;
	public final static int EXPANSE_XLARGE 	= 203;
	public final static int EXPANSE_XSMALL	= 204;
	
	protected final static BasisNumber BASISNUMBER_1 = BasisNumber.createBasisNumber(1);
	protected final static BasisNumber BASISNUMBER_25 = BasisNumber.createBasisNumber(25);
	protected final static BasisNumber BASISNUMBER_250 = BasisNumber.createBasisNumber(250);
	
	protected BBjControl ctrl;
	
	public abstract void create(IPanel p);
	
	public String getText() {
		try {
			return ctrl.getText();
		} catch (BBjException e) {
			e.printStackTrace();
		}
		return "";
	}
	
	
	@Override
	public void setExpanse(int expanse) {
		try {
			switch (expanse) {
				case EXPANSE_LARGE:
					ctrl.setAttribute("expanse", "l");
					break;
				case EXPANSE_MEDIUM:
					ctrl.setAttribute("expanse", "m");
					break;
				case EXPANSE_SMALL:
					ctrl.setAttribute("expanse", "s");
					break;
				case EXPANSE_XLARGE:
					ctrl.setAttribute("expanse", "xl");
					break;
				case EXPANSE_XSMALL:
					ctrl.setAttribute("expanse", "xs");
					break;			

				default:
					//noop
				}
			} catch (BBjException e) {
				e.printStackTrace();
			}
		
	}
	
	@Override
	public void setTheme(int theme) {
		try {
			switch (theme) {
				case THEME_DEFAULT:
					ctrl.setAttribute("theme", "default");
					break;
				case THEME_DANGER:
					ctrl.setAttribute("theme", "danger");
					break;
				case THEME_GRAY:
					ctrl.setAttribute("theme", "gray");
					break;
				case THEME_INFO:
					ctrl.setAttribute("theme", "info");
					break;
				case THEME_PRIMARY:
					ctrl.setAttribute("theme", "primary");
					break;			
				case THEME_SUCCESS:
					ctrl.setAttribute("theme", "success");
					break;
				case THEME_WARNING:
					ctrl.setAttribute("theme", "warning");
					break;	
				default:
					//noop
				}
			} catch (BBjException e) {
				e.printStackTrace();
			}
	}
	
	
	@Override
	public void setStyle(String property, String value) {
		try {
			ctrl.setStyle(property, value);
		} catch (BBjException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void addClass(String selector) {
		try {
			ctrl.addStyle(selector);
		} catch (BBjException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void removeClass(String selector) {
		try {
			ctrl.removeStyle(selector);
		} catch (BBjException e) {
			e.printStackTrace();
		}
	}

	
}
