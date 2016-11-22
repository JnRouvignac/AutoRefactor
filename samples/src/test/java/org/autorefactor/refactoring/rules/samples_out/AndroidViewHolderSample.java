/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2016 Luis Cruz - Android Refactoring
 * Copyright (C) 2016 Jean-Noël Rouvignac - code cleanups
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.  If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.refactoring.rules.samples_out;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

public abstract class AndroidViewHolderSample extends BaseAdapter {
    @Override
    public int getCount() {
        return 0;
    }

    @Override
    public Object getItem(int position) {
        return null;
    }

    @Override
    public long getItemId(int position) {
        return 0;
    }

    /** Nothing to refactor: no perf impact. */
    public static class AdapterOk extends AndroidViewHolderSample {
        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            return null;
        }
    }

    /** Refactor to use the view holder pattern. */
    public static class AdapterUsingRecycledView extends AndroidViewHolderSample {
        LayoutInflater mInflater;

        private static class ViewHolderItem {
            private TextView text;
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            ViewHolderItem viewHolderItem;
            if (convertView == null) {
                convertView = mInflater.inflate(R.layout.your_layout, null);
                viewHolderItem = new ViewHolderItem();
                viewHolderItem.text = (TextView) convertView
                        .findViewById(R.id.text);
                convertView.setTag(viewHolderItem);
            } else {
                viewHolderItem = (ViewHolderItem) convertView.getTag();
            }
            TextView text = viewHolderItem.text;
            text.setText("Position " + position);

            return convertView;
        }
    }

    /** Refactor to use the view holder pattern. */
    public static class AdapterNotReturningRecycledView extends AndroidViewHolderSample {
        LayoutInflater mInflater;

        private static class ViewHolderItem {
            private TextView text;
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            ViewHolderItem viewHolderItem;
            if (convertView == null) {
                convertView = mInflater.inflate(R.layout.your_layout, null);
                viewHolderItem = new ViewHolderItem();
                viewHolderItem.text = (TextView) convertView
                        .findViewById(R.id.text);
                convertView.setTag(viewHolderItem);
            } else {
                viewHolderItem = (ViewHolderItem) convertView.getTag();
            }
            TextView text = viewHolderItem.text;
            text.setText("Position " + position);

            return convertView;
        }
    }

    /** Refactor to use the view holder pattern. */
    public static class AdapterUsingDifferentView extends AndroidViewHolderSample {
        LayoutInflater mInflater;

        private static class ViewHolderItem {
            private TextView text;
        }

        @Override
        public View getView(int position, View convertView, ViewGroup parent) {
            ViewHolderItem viewHolderItem;
            if (convertView == null) {
                convertView = mInflater.inflate(R.layout.your_layout, null);
                viewHolderItem = new ViewHolderItem();
                viewHolderItem.text = (TextView) convertView
                        .findViewById(R.id.text);
                convertView.setTag(viewHolderItem);
            } else {
                viewHolderItem = (ViewHolderItem) convertView.getTag();
            }
            View v = convertView;
            TextView text = viewHolderItem.text;
            text.setText("Position " + position);

            return v;
        }
    }

    /** Do not refactor: it already uses the view holder pattern. */
    public static class AdapterUsingViewHolder extends AndroidViewHolderSample {
        LayoutInflater mInflater;

        public View getView(int position, View convertView, ViewGroup parent) {
            // Already using View Holder pattern
            convertView = convertView == null ? mInflater.inflate(R.layout.your_layout, null) : convertView;

            TextView text = (TextView) convertView.findViewById(R.id.text);
            text.setText("Position " + position);

            return convertView;
        }
    }

    public static class AdapterUsingViewHolderSwitch extends AndroidViewHolderSample {
        LayoutInflater inflater;

        @Override
        public View getView(final int position, final View convertView, final ViewGroup parent) {
            View rootView = convertView;
            final int itemViewType = getItemViewType(position);
            switch (itemViewType) {
            case 0:
                if (rootView != null) {
                    return rootView;
                }
                rootView = inflater.inflate(android.R.layout.simple_list_item_1, parent, false);
                break;
            }
            return rootView;
        }
    }

    /** TODO low priority ViewHolder cornercase. */
    public static class CornerCase extends AndroidViewHolderSample {
        LayoutInflater inflater;

        @Override
        public View getView(final int position, View convertView, final ViewGroup parent) {
            View rootView = convertView;
            // this should not be refactored
            if (rootView != null) {
                return rootView;
            }
            if (convertView == null) {
                convertView = inflater.inflate(android.R.layout.simple_list_item_1, parent, false);
            }
            rootView = convertView;
            return rootView;
        }
    }

    /**
     * GUI resources are defined using XML and can have the attribute {@code id}.
     * <p>
     * This {@code id} is later used to get that resource in the code.
     * <p>
     * {@code R.id.<id_of_resource>} is the way it's done.
     */
    private static class R {
        public static class layout {
            public static final int your_layout = 2;
        }

        public static class id {
            public static final int text = 2;
        }
    }
}
