package org.dwcj.bbj.database;

import org.dwcj.Environment;

import java.sql.Connection;

public class JDBCConnection {

    private JDBCConnection() {}

    public static Connection getJDBCConnection(String databasename) throws Exception {
        return Environment.getInstance().getBBjAPI().getJDBCConnection(databasename);
    }
}
