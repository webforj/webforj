package org.dwcj;

import java.net.URL;
import java.util.HashMap;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjSysGui;
import com.basis.startup.type.BBjException;

public class Environment {

	private BBjAPI api;
	private BBjSysGui sysgui;
	private IDwcjHelper helper;

	private static HashMap<Object,Environment> instanceMap = new HashMap<>();

	
	private Environment(BBjAPI api, IDwcjHelper helper) throws BBjException {
		this.api = api;
		this.sysgui = api.openSysGui("X0");
		this.helper = helper;
	}
	
	public static void init(BBjAPI api, IDwcjHelper helper) throws BBjException {
		Environment env = new Environment(api,helper); 
		Environment.instanceMap.put(Thread.currentThread().getName(), env);
	}
	
	public static void cleanup() {
		Environment.instanceMap.remove(Thread.currentThread().getName());
	}
	
	public static Environment getInstance() {
		return Environment.instanceMap.get(Thread.currentThread().getName());
	}
	
	public BBjAPI getBBjAPI() {
		return this.api;
	}

	public BBjSysGui getSysGui() {
		return this.sysgui;
	}

	public IDwcjHelper  getDwcjHelper() {
		return helper;
	}

}
