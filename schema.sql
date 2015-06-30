CREATE TABLE users (
        id integer primary key autoincrement not null,
        email text not null,
        login text unique,
        inviter integer references users,
        vote integer
        );
CREATE TABLE messages (
        id integer primary key autoincrement not null,
        message text not null,
        user integer not null references users(id),
        type integer not null,
        parent integer references messages(id),
        conversation integer,
        vote integer not null default 0,
        data string not null default (datetime('now'))
        );
create table conversations (
        id integer primary key autoincrement not null,
        tail integer not null references messages(id) ,
        head integer not null references messages(id) ,
        count integer not null
        );
        
        
CREATE TABLE voting (
        message integer references messages(id)  on delete cascade,
        user integer references users(id),
        unique (message,user)
        );
CREATE TABLE store (
        message integer references messages(id) on delete cascade,
        user integer references users(id),
        unique (message,user)
        );
CREATE INDEX voting1 on voting (message,user);
CREATE INDEX voting2 on voting (message);
CREATE INDEX store1 on store (message,user);
CREATE INDEX store2 on store (user);
CREATE INDEX store3 on store (message);
CREATE TABLE client (
        id integer primary key autoincrement not null,
        user integer references users(id),
        message integer references messages(id) on delete cascade,
        data string not null,
        unique (message,user)
        );
create table blobs (
        client integer references client(id) on delete cascade,
        interface string not null,
        blob blob
        );
create table labels (
        client integer references client(id) on delete cascade,
        label string not null
        );
        
CREATE INDEX client1 on client (user);
CREATE INDEX client2 on client (data);
CREATE INDEX blobs1 on blobs (client);
CREATE INDEX blobs2 on blobs (interface);
