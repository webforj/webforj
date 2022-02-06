package org.dwcj;

import com.basis.startup.type.CustomObject;

public interface IDwcjHelper {
	
	 public CustomObject getEventProxy(Object obj, String method);
	 public int msgbox(String msg, int options, String title);

}
