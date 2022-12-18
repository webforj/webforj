package org.dwcj.environment.namespace;

import com.basis.bbj.proxies.BBjObjectTable;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import org.dwcj.Environment;
import org.dwcj.interfaces.HasStyle;

import java.util.HashSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

public class SessionNamespace implements Namespace
{
    private BBjObjectTable otable = Environment.getInstance().getBBjAPI().getObjectTable();

    @Override
    public void put(String key, Object value) {
        otable.put(key,value);
    }

    @Override
    public Object get(String key) {
        try {
            return otable.get(key);
        } catch (BBjException e) {
            throw new NoSuchElementException("Element "+key+" does not exist.");
        }
    }

    @Override
    public void remove(String key) {
        otable.remove(key);
    }

    @Override
    public Set<String> keySet() {
        BBjVector tmp = otable.getKeys();
        Iterator it = tmp.iterator();
        HashSet<String> keyset = new HashSet<>();
        while (it.hasNext())
            keyset.add(it.next().toString());
        return keyset;
    }

    @Override
    public int size() {
        return otable.size();
    }

    @Override
    public void clear() {
        otable.clear();
    }
}
