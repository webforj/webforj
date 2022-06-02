package org.dwcj.bbj.database;

import com.basis.startup.type.BBjException;
import org.dwcj.Environment;

import java.sql.Connection;

public class JDBCConnection {
    public static Connection getJDBCConnection(String databasename) throws Exception {
        return Environment.getInstance().getBBjAPI().getJDBCConnection(databasename);
    }
}
