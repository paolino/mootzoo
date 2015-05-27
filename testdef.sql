create table users (
        id integer primary key autoincrement not null,
        email text not null,
        login text unique
        );
create table conversations (
        id integer primary key autoincrement not null,
        pusher integer not null,
        message text not null,
        opponent integer,
        foreign key(pusher) references users(id),
        foreign key(opponent) references users(id)
        );


create table messages (
        id integer primary key autoincrement not null,
        message text not null,
        conversation integer not null,
        foreign key(conversation) references conversations(id)
        );

create table interests (
        user integer,
        conversation integer,
        value integer,
        foreign key(user) references users(id),
        foreign key(conversation) references conversations(id),
        unique(user,conversation)
        );

-- create view proposals as select id from conversations where opponent isnull;
--------------------------------        
-- joining conversation from an opponent consistency: cannot substitute an opponent and cannot be the same as pusher
--------------------------------
create view joinconversation as select conversations.id as conversation, opponent from conversations;

create trigger joinconversation instead of insert on joinconversation begin
        select case when ((select id from conversations where id = new.conversation) isnull) then raise (abort,"not existing conversation") end;
        select case when ((select opponent from conversations where id = new.conversation) notnull) then raise (abort,"conversation already taken") end;
        select case when ((select pusher from conversations where id = new.conversation and new.opponent = pusher) notnull) then raise (abort,"opponent same as pusher") end;
        update conversations set opponent = new.opponent where conversations.id = new.conversation;
        end;

--------------------------------
-- dropping a conversation from an opponent: must be taken from the opponent in the opponent position
--------------------------------
create view dropconversation as select conversations.id as conversation, opponent from conversations;

create trigger dropconversation instead of insert on dropconversation begin
        select case when ((select id from conversations where id = new.conversation) isnull) then raise (abort,"not existing conversation") end;
        select case when ((select opponent from conversations where id = new.conversation) isnull) then raise (abort,"conversation still not taken") end;
        select case when ((select opponent from conversations where id = new.conversation and new.opponent = opponent) isnull) then raise (abort,"not the opponent") end;
        update conversations set opponent = null where conversations.id = new.conversation;

        end;

-----------------------------------
-- inserting a new message: must be the opponent, opponent and pusher positions are switched in the conversation, the message is inserted in the conversation and the old message is moved in the messages table.
---------------------------------
create view newmessage as select message,id as conversation,pusher from conversations;

create trigger newmessage instead of insert on newmessage begin

        select case when ((select id from conversations where id = new.conversation) isnull) then raise (abort,"not existing conversation") end;
        select case when ((select opponent from conversations where id = new.conversation and opponent = new.pusher ) isnull) then raise (abort,"not the opponent") end;
        insert into messages (conversation,message) select new.conversation, message from conversations where id = new.conversation;
        replace into conversations (id,pusher,message,opponent) select id,opponent, new.message,pusher from conversations where id=new.conversation;
        end;


        
