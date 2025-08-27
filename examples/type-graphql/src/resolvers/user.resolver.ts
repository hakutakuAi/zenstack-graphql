import { Resolver, Query, Mutation, Arg, Ctx, FieldResolver, Root, Int, Info } from 'type-graphql'
import type { GraphQLResolveInfo } from 'graphql'
import { User, Post, Comment, UserFilterInput, UserSortInput, UserConnection, UserQueryArgs } from '../../schema'
import { BaseResolver } from './base-resolver'
import type { Context } from './types'
import { ConnectionBuilder, POST_INCLUDES, COMMENT_INCLUDES } from '../../schema-helpers'

@Resolver(() => User)
export class UserResolver extends BaseResolver {
	@Query(() => UserConnection)
	async users(
		@Arg('filter', () => UserFilterInput, { nullable: true }) filter: UserFilterInput | undefined,
		@Arg('sort', () => UserSortInput, { nullable: true }) sort: UserSortInput | undefined,
		@Arg('first', () => Int, { nullable: true }) first: number | undefined,
		@Arg('after', () => String, { nullable: true }) after: string | undefined,
		@Arg('last', () => Int, { nullable: true }) last: number | undefined,
		@Arg('before', () => String, { nullable: true }) before: string | undefined,
		@Info() info: GraphQLResolveInfo,
		@Ctx() { prisma }: Context,
	): Promise<UserConnection> {
		const args: UserQueryArgs = { filter, sort, first, after, last, before }
		return ConnectionBuilder.buildUserConnection(prisma, args, info)
	}

	@Query(() => User, { nullable: true })
	async user(@Arg('id', () => String) id: string, @Ctx() { prisma }: Context): Promise<User | null> {
		return this.findUnique<User>(prisma, 'user', { id })
	}

	@Mutation(() => User)
	async createUser(
		@Arg('name', () => String) name: string,
		@Arg('email', () => String) email: string,
		@Arg('bio', () => String, { nullable: true }) bio: string | undefined,
		@Ctx() { prisma }: Context,
	): Promise<User> {
		return this.create<User>(prisma, 'user', { name, email, bio })
	}

	@FieldResolver(() => [Post])
	async posts(@Root() user: User, @Ctx() { prisma }: Context): Promise<Post[]> {
		return this.findMany<Post>(prisma, 'post', {
			where: { authorId: user.id },
			include: POST_INCLUDES,
		})
	}

	@FieldResolver(() => [Comment])
	async comments(@Root() user: User, @Ctx() { prisma }: Context): Promise<Comment[]> {
		return this.findMany<Comment>(prisma, 'comment', {
			where: { authorId: user.id },
			include: COMMENT_INCLUDES,
		})
	}
}
