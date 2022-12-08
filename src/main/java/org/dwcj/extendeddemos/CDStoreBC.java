package org.dwcj.extendeddemos;

import com.basiscomponents.bc.SqlTableBC;
import org.dwcj.bbj.database.JDBCConnection;

import java.sql.Connection;
import java.sql.SQLException;

public class CDStoreBC extends SqlTableBC {
    public CDStoreBC() throws Exception {
        super(JDBCConnection.getJDBCConnection("CDStore"));
        setTable("CDINVENTORY");
    }
}
