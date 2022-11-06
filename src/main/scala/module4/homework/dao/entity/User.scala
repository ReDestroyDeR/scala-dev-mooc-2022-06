package module4.homework.dao.entity

case class Role(
    code: String,
    name: String
){
    def typedCode: RoleCode = RoleCode(code)
}

case class User(
    id: String,
    firstName: String,
    lastName: String,
    age: Int
){
    def typedId: UserId = UserId(id)
}

case class UserToRole(roleId: RoleCode, userId: UserId)

case class RoleCode(code: String) extends AnyVal
case class UserId(id: String) extends AnyVal