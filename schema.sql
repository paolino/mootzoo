CREATE TABLE users (
        id integer primary key autoincrement not null,
        email text not null,
        login text unique,
        inviter integer references users
        );
CREATE TABLE messages (
        id integer primary key autoincrement not null,
        message text not null,
        user integer not null references users(id),
        type integer not null,
        parent integer references messages(id),
        conversation integer,
        vote integer not null default 0
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
