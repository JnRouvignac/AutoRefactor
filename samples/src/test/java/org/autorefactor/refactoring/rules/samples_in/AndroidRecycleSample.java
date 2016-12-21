package org.autorefactor.refactoring.rules.samples_in;

import java.util.ArrayList;
import java.util.List;

import android.content.ContentProvider;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.res.TypedArray;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.net.Uri;
import android.os.Message;
import android.os.RemoteException;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.VelocityTracker;
import android.view.View;
import android.content.Context;
import android.os.Parcel;

public class AndroidRecycleSample {
    public void cursorError1(SQLiteDatabase db, long route_id) {
        Cursor cursor = db.query("TABLE_TRIPS",
                new String[]{ "KEY_TRIP_ID" },
                "ROUTE_ID=?",
                new String[]{Long.toString(route_id)},
                null, null, null);
    }

    void testProviderQueries(Uri uri, ContentProvider provider, ContentResolver resolver, ContentProviderClient client)
            throws RemoteException {
        Cursor query = provider.query(uri, null, null, null, null);
        Cursor query2 = resolver.query(uri, null, null, null, null);
        Cursor query3 = client.query(uri, null, null, null, null);
    }

    public int ok(SQLiteDatabase db, long route_id, String table, String whereClause, String id) {
        int total_deletions = 0;
        Cursor cursor = db.query("TABLE_TRIPS",
                new String[]{ "KEY_TRIP_ID" },
                "ROUTE_ID" + "=?",
                new String[]{Long.toString(route_id)},
                null, null, null);

        while (cursor.moveToNext()) {
            total_deletions += db.delete(table, whereClause + "=?",
                    new String[]{Long.toString(cursor.getLong(0))});
        }
        return total_deletions;
    }

    public Cursor getCursor(SQLiteDatabase db) {
        Cursor cursor = db.query("TABLE_TRIPS",
                new String[]{ "KEY_TRIP_ID" },
                "ROUTE_ID" + "=?",
                new String[]{Long.toString(5)},
                null, null, null);
        return cursor;
    }
    
    public String testResourceUsedInReturn(SQLiteDatabase db) {
        Cursor cursor = db.query("TABLE_TRIPS",
                new String[]{ "KEY_TRIP_ID" },
                "ROUTE_ID" + "=?",
                new String[]{Long.toString(5)},
                null, null, null);
        return cursor.getString(0);
    }

    public void testMultipleAssignment(Uri uri, ContentProvider provider) {
        Cursor query = provider.query(uri, null, null, null, null);
        query.getLong(0);
        query = provider.query(uri, null, null, null, null);
        query.close();
    }

    public class RecycleTest extends View {
        public RecycleTest(Context context, AttributeSet attrs, int defStyle) {
            super(context, attrs, defStyle);
        }

        public void wrong1(AttributeSet attrs, int defStyle) {
            final TypedArray a = getContext().obtainStyledAttributes(attrs, new int[] { 0 }, defStyle, 0);
            String example = a.getString(0);
        }

        public void wrong2(AttributeSet attrs, int defStyle) {
            final TypedArray a = getContext().obtainStyledAttributes(new int[0]);
        }

        // ---- Check recycling VelocityTracker ----

        public void tracker() {
            VelocityTracker tracker = VelocityTracker.obtain();
        }

        // ---- Check recycling Message ----

        public void message() {
            Message message1 = getHandler().obtainMessage();
            Message message2 = Message.obtain();
        }

        // ---- Check recycling MotionEvent ----

        public void motionEvent() {
            MotionEvent event1 = MotionEvent.obtain(null);
            MotionEvent event2 = MotionEvent.obtainNoHistory(null);
        }

        public void motionEvent2() {
            MotionEvent event1 = MotionEvent.obtain(null); // OK
            MotionEvent event2 = MotionEvent.obtainNoHistory(null); // Not recycled
            event1.recycle();
        }

        public void motionEvent3() {
            MotionEvent event1 = MotionEvent.obtain(null); // Not recycled
            MotionEvent event2 = MotionEvent.obtain(event1);
            event2.recycle();
        }

        // ---- Check recycling Parcel ----
        public void parcelMissing() {
            Parcel myparcel = Parcel.obtain();
            myparcel.createBinderArray();
        }

        // No refactor to do here
        public boolean testTryWithResources(SQLiteDatabase db) {
            try (Cursor cursor = db.query("TABLE", new String[] { "KEY_TIMESTAMP" }, null, null, null, null, null,
                    "1")) {
                return cursor.moveToFirst();
            }
        }
    }

    public static String testRecycleBeforeEarlyReturns(Context context, Uri uri) {
        if ("content".equalsIgnoreCase(uri.getScheme())) {
            String[] projection = { "_data" };
            Cursor cursor = null;

            try {
                cursor = context.getContentResolver().query(uri, projection, null, null, null);
                int column_index = cursor
                .getColumnIndexOrThrow("_data");
                if (cursor.moveToFirst()) {
                    return cursor.getString(column_index);
                }
            } catch (Exception e) {
            }
        }
        else if ("file".equalsIgnoreCase(uri.getScheme())) {
            return uri.getPath();
        }
        return null;
    }

    public static int getCalendars(Context context, boolean onlyWritable) {
        List<Integer> ids = new ArrayList<>();
        List<String> names = new ArrayList<>();
        List<String> displayNames = new ArrayList<>();
        List<String> accountNames = new ArrayList<>();
        ContentResolver cr = context.getContentResolver();

        Cursor c = null;
        try {
            c = cr.query(Uri.parse("com.example.android"),
                    new String[]{"CALENDAR_PROJECTION"}, null, null, null);
            if (c != null) {
                while (c.moveToNext()) {
                    int COLUMN_CAL_ACCESS_LEVEL = 0;
                    if (!onlyWritable || (1 == (c.getInt(COLUMN_CAL_ACCESS_LEVEL)))) {
                        int COLUMN_CAL_ID = 0;
                        int id = c.getInt(COLUMN_CAL_ID);
                        int COLUMN_CAL_NAME = 0;
                        int COLUMN_CAL_DISPLAY_NAME = 0;
                        int COLUMN_CAL_ACCOUNT_NAME = 0;
                        String name = c.getString(COLUMN_CAL_NAME);
                        String displayName = c.getString(COLUMN_CAL_DISPLAY_NAME);
                        String accountName = c.getString(COLUMN_CAL_ACCOUNT_NAME);

                        ids.add(id);
                        names.add(name);
                        displayNames.add(displayName);
                        accountNames.add(accountName);
                    }
                }
            }
        } catch (Exception e) {
//            Analytics.sendException(context, e, false);
        } finally {
            if (c != null) c.close();
        }

        return 0;
    }

    private static class R {
        public static class styleable {
            public static final int WeekView_scrollDuration = 1;
            public static final int[] WeekView = {2};
        }
    }

    public class ContentProviderClientTest {
        public void error1(ContentResolver resolver) {
            ContentProviderClient client = resolver.acquireContentProviderClient("test");
        }
    }
}
