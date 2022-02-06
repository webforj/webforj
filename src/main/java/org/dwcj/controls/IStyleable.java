package org.dwcj.controls;

public interface IStyleable {
	
	public void setStyle(String property, String value);
	public void addClass(String selector);
	public void removeClass(String selector);

}
